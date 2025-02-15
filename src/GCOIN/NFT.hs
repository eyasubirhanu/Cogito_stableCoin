{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GCOIN.NFT
  ( gcoinTokenName,
    ownerOref,
    validator,
    apiExamplePlutusMintingScript,
    scriptAsCbor,
    validator,
    mkTokenPolicy,
    tokenPolicy,
    tokenCurSymbol,
    plutusScript,
    validator,
    scriptAsCbor,
    apiExamplePlutusMintingScript,
    mintingScriptShortBs,
  )
where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise
import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Short qualified as SBS
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.OpenApi.Schema (ToSchema)
import Data.Text (Text, pack)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger hiding (mint, singleton)
import Ledger.Constraints as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value as Value
import Plutus.Contract as Contract
import Plutus.Contract.Wallet (getUnspentOutput)
import Plutus.Script.Utils.V1.Scripts (scriptCurrencySymbol)
import Plutus.Trace.Emulator as Emulator
import Plutus.V1.Ledger.Scripts (Script, Validator (Validator), mkMintingPolicyScript, unMintingPolicyScript)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Text.Printf (printf)
import Wallet.Emulator.Wallet
import Prelude (Semigroup (..), Show (..), String)
import Prelude qualified hiding (($))

-- creating a tokenname
{-# INLINEABLE gcoinTokenName #-}
gcoinTokenName :: TokenName
gcoinTokenName = tokenName "GNFT"

ownerOref :: TxOutRef
ownerOref = TxOutRef "05675c13f2289c444fc66f4bc694db2949ee9fe8b6e182e43fb083957c9db780" 2

{-# INLINEABLE mkTokenPolicy #-}
mkTokenPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkTokenPolicy oref tn () ctx =
  traceIfFalse "UTxO not consumed" hasUTxO
    && traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
      [(cs, tn', amt')] -> cs == ownCurrencySymbol ctx && tn == tn' && amt' == 1
      _ -> False

tokenPolicy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
tokenPolicy oref tn =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||\oref' tn' -> Scripts.mkUntypedMintingPolicy $ mkTokenPolicy oref' tn'||])
      `PlutusTx.applyCode` PlutusTx.liftCode oref
      `PlutusTx.applyCode` PlutusTx.liftCode tn

tokenCurSymbol :: TxOutRef -> TokenName -> CurrencySymbol
tokenCurSymbol oref tn = scriptCurrencySymbol $ tokenPolicy oref tn

plutusScript :: TxOutRef -> TokenName -> Script
plutusScript oref tn = unMintingPolicyScript $ tokenPolicy oref tn

validator :: TxOutRef -> TokenName -> Validator
validator oref tn = Validator $ unMintingPolicyScript $ tokenPolicy oref tn

scriptAsCbor :: TxOutRef -> TokenName -> LB.ByteString
scriptAsCbor oref tn = serialise $ validator oref tn

apiExamplePlutusMintingScript :: TxOutRef -> TokenName -> PlutusScript PlutusScriptV1
apiExamplePlutusMintingScript oref tn = PlutusScriptSerialised . SBS.toShort $ LB.toStrict (scriptAsCbor oref tn)

mintingScriptShortBs :: TxOutRef -> TokenName -> SBS.ShortByteString
mintingScriptShortBs oref tn = SBS.toShort . LB.toStrict $ scriptAsCbor oref tn