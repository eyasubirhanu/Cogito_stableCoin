{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
module GCOIN.Mint where
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise
import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Short qualified as SBS
import Data.Monoid (Last (..), (<>))
import Data.Text (Text)
import Data.Void (Void)
import GCOIN.HandlerContract
import GHC.Generics (Generic)
import Ledger (CurrencySymbol, PaymentPubKey, PaymentPubKeyHash (unPaymentPubKeyHash), TxId, TxOutRef (..), getCardanoTxId, pubKeyHashAddress)
import Ledger hiding (mint, singleton)
import Ledger.Ada (Ada)
import Ledger.Ada qualified as Ada
import Ledger.Constraints as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value as Value
import Plutus.Contract as Contract
import Plutus.Script.Utils.V1.Scripts (scriptCurrencySymbol)
import Plutus.Trace.Emulator as Emulator
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Contexts
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Text.Printf (printf)
import Wallet.Emulator.Wallet
import Prelude (IO, Semigroup (..), Show (..), String, div)
import Prelude qualified
import Ledger.Value (assetClassValue)

-- creating a tokenname
{-# INLINEABLE gcoinTokenName #-}
gcoinTokenName :: TokenName
gcoinTokenName = tokenName "GCOIN"

data MintRedeemer = MintCoin Integer | BurnCoin Integer PaymentPubKeyHash
  deriving (Show)
PlutusTx.makeIsDataIndexed ''MintRedeemer [('MintCoin, 0), ('BurnCoin, 1)]
{-# INLINEABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue
{-# INLINEABLE mkPolicy #-}
mkPolicy :: Handler -> TokenName -> Address -> MintRedeemer -> ScriptContext -> Bool
mkPolicy handler tn addr re ctx =
  case re of
    MintCoin mintAmount -> traceIfFalse "insufficient ada value to mint" $ checkMintValue (calculateValue mintAmount mintRate) --is add paid to handler in output
    BurnCoin burnAmount user ->
      traceIfFalse "The amount is a positive number. " (burnAmount < 0)
        && traceIfFalse "handler not paying insufficient ada value to burn" (checkBurnReturnValue (calculateValue burnAmount burnRate)) --is ada subtracted to handler in output
        && traceIfFalse "GCOIN in the specified quantity was not burned." (checkBurnValue user burnAmount) --is user burn token
        && traceIfFalse "Invalid ada value " (feesPaid user burnAmount burnRate) --is user get paid
  where
    info :: TxInfo -- Creating an instance to access the pending transactions and related types.
    info = scriptContextTxInfo ctx
    -- function to get the input utxo from the ScriptContext(in handler script)
    ownInput :: TxOut
    ownInput =
      let ins =
            [ o
              | i <- txInfoInputs info,
                let o = txInInfoResolved i,
                inputHasNFT o
            ]
       in case ins of
            [o] -> o
            _ -> traceError "expected exactly one handler input"
    -- function to get the input utxo from the ScriptContext(in handler sctipt)
    ownOutput :: TxOut
    ownOutput =
      let ins =
            [ o
              | o <- txInfoOutputs info,
                inputHasNFT o
            ]
       in case ins of
            [o] -> o
            _ -> traceError "expected exactly one handler input"

    handlerValue' :: HandlerDatum
    handlerValue' = case handlerValue (txOutDatumHash ownInput >>= flip findDatum info) of
      Nothing -> traceError "handler value not found"
      Just x -> x

    getInput :: PaymentPubKeyHash -> [TxOut]
    getInput pkh =
      let os =
            [ txInInfoResolved i
              | i <- txInfoInputs info,
                let o = txInInfoResolved i,
                checkpkh (txOutAddress o) pkh
            ]
       in case os of
            [] -> traceError "expected exactly one inputs from given PaymentPubKeyHash"
            _ -> os
    getOutput :: PaymentPubKeyHash -> Bool
    getOutput pkh =
      let os =
            [ o
              | o <- txInfoOutputs info,
                checkpkh (txOutAddress o) pkh
            ]
       in case os of
            [] -> traceError "expected exactly one outputs from given PaymentPubKeyHash"
            _ -> True

    
    getsInValue :: PaymentPubKeyHash -> Value -> Bool
    getsInValue h v =
      let
        [o] = [ o'
              | o' <- txInfoOutputs info
              , txOutValue o' == v
              ]
      in
        txOutAddress o == pubKeyHashAddress h Nothing
    
  


    inputHasNFT :: TxOut -> Bool
    inputHasNFT i = assetClassValueOf (txOutValue i) (handlerAsset handler) == 1

    --fucntion to calculte how much value(ada) is needed to mint a given amount 
    calculateValue :: Integer -> Integer -> Integer
    calculateValue r amount = r * amount

    getTokenValue :: [TxOut] -> Integer
    getTokenValue (x : xs) = valueOf (txOutValue x) (ownCurrencySymbol ctx) tn + getTokenValue xs
    getTokenValue [] = 0

    getlovelace ::  [TxOut] -> Integer
    getlovelace (x : xs) = lovelaces (txOutValue x) + getlovelace xs
    getlovelace []  = 0

    checkMintValue :: Integer -> Bool
    checkMintValue amount = txOutValue ownOutput == txOutValue ownInput <> Ada.lovelaceValueOf amount
    --take nig calculate value and add it to input 
    checkBurnReturnValue :: Integer -> Bool
    checkBurnReturnValue amount = txOutValue ownOutput == txOutValue ownInput <> Ada.lovelaceValueOf amount

    checkpkh :: Address -> PaymentPubKeyHash -> Bool
    checkpkh addr' pkh' = case toPubKeyHash addr' of
      Nothing -> False
      Just x -> x == unPaymentPubKeyHash pkh'

    checkBurnValue :: PaymentPubKeyHash -> Integer -> Bool
    checkBurnValue pkh amount = outputValue == getTokenValue (getInput pkh) + amount --Todo get valueof token in pkh
      where
        userOutput = valuePaidTo info (unPaymentPubKeyHash pkh) 
        outputValue = valueOf userOutput (ownCurrencySymbol ctx) tn --valueof token paid by this tx to pkh

    feesPaid :: PaymentPubKeyHash -> Integer -> Integer -> Bool
    feesPaid pkh amount rate = outVal == (inVal +  calculateValue rate (abs amount)) - lovelaces (txInfoFee info)
      where
        outVal = lovelaces $ valuePaidTo info (unPaymentPubKeyHash pkh)
        inVal =  getlovelace (getInput pkh)
  

    burnRate :: Integer
    burnRate = exchangeRate handlerValue' - divide (exchangeRate handlerValue') 100

    mintRate :: Integer
    mintRate = exchangeRate handlerValue' + divide (exchangeRate handlerValue') 100

  
wrappedPolicy :: Handler -> TokenName -> Address -> BuiltinData -> BuiltinData -> ()
wrappedPolicy handler tn addr re ctx =
  check
    ( mkPolicy
        handler
        tn
        addr
        (PlutusTx.unsafeFromBuiltinData re)
        (PlutusTx.unsafeFromBuiltinData ctx)
    )

policy :: Handler -> TokenName -> Address -> MintingPolicy
policy handler tn addr =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||wrappedPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode handler
      `PlutusTx.applyCode` PlutusTx.liftCode tn
      `PlutusTx.applyCode` PlutusTx.liftCode addr

curSymbol :: Handler -> TokenName -> Address -> CurrencySymbol
curSymbol handler tn addr = scriptCurrencySymbol $ policy handler tn addr

validator :: Handler -> TokenName -> Address -> Validator
validator handler tn addr =
  Validator $ unMintingPolicyScript $ policy handler tn addr
  
plutusScript :: Handler -> TokenName -> Address -> Script
plutusScript handler tn addr =unMintingPolicyScript $ policy handler tn addr

scriptAsCbor :: Handler -> TokenName -> Address -> LB.ByteString
scriptAsCbor handler tn addr = serialise $ validator handler tn addr

apiExamplePlutusMintingScript :: Handler -> TokenName -> Address -> PlutusScript PlutusScriptV1
apiExamplePlutusMintingScript handler tn addr = PlutusScriptSerialised $ SBS.toShort $ LB.toStrict $ scriptAsCbor handler tn addr

gcoinScriptShortBs :: Handler -> TokenName -> Address -> SBS.ShortByteString
gcoinScriptShortBs handler tn addr = SBS.toShort $ LB.toStrict $ scriptAsCbor handler tn addr