{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module GCOIN.Deploy where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified GCOIN.NFT as Mint
import qualified GCOIN.HandlerContract as Handle 
import qualified GCOIN.Mint as Gcoin 
import qualified Ledger
import Plutus.V1.Ledger.Api (toData)
import PlutusTx (Data (..), ToData)


writeValidator :: FilePath -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing $ Mint.apiExamplePlutusMintingScript Mint.ownerOref Mint.gcoinTokenName

writeMintingValidatorScript :: IO (Either (FileError ()) ())
writeMintingValidatorScript = writeValidator "output/my-NFT-1-script.plutus"



writeHandlerValidator :: FilePath -> IO (Either (FileError ()) ())
writeHandlerValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing $ Handle.apiExamplePlutusMintingScript Handle.handlerParam

writeHandlingValidatorScript :: IO (Either (FileError ()) ())
writeHandlingValidatorScript = writeHandlerValidator "output/my-handle-1-script.plutus"



writeGcoinValidator :: FilePath -> IO (Either (FileError ()) ())
writeGcoinValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing $ Gcoin.apiExamplePlutusMintingScript Handle.handlerParam Gcoin.gcoinTokenName $ Handle.handlerAddress Handle.handlerParam

writeGcoinValidatorScript :: IO (Either (FileError ()) ())
writeGcoinValidatorScript = writeGcoinValidator "output/my-gcoinMint-1-script.plutus"