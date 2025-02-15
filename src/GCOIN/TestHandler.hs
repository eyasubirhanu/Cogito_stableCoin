{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GCOIN.TestHandler where

import Control.Monad hiding (fmap)
import Control.Monad.Freer.Extras ()
import qualified Control.Monad.Freer.Extras as Extras
import Data.Default (Default (..))
import qualified Data.Map as Map
import Data.Monoid (Last (..))
import Data.Text (Text)
import GCOIN.HandlerContract
import Ledger
import Ledger.Ada as Ada
import Ledger.Value ()
import Plutus.Contract as Contract hiding (when)
import Plutus.Trace.Emulator as Emulator
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Wallet.Emulator.Wallet
import Prelude (IO, Semigroup (..), show)

test :: IO ()
test = runEmulatorTraceIO' def emCfg myTrace
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig (Left $ Map.fromList $ [(knownWallet i, v) | i <- [1 .. 4]]) def
    v :: Value
    v = Ada.lovelaceValueOf 10000000

checkHandler :: Handler -> Contract () w Text ()
checkHandler handler = do
  m <- findHandlerOutput handler
  case m of
    Nothing -> return ()
    Just (_, _, x) -> Contract.logInfo $ "Handler value: " ++ show x
  Contract.waitNSlots 1 >> checkHandler handler

myTrace :: EmulatorTrace ()
myTrace = do
  h1 <- activateContractWallet (knownWallet 1) $ runhandler

  void $ Emulator.waitNSlots 1
  void $ getHandler h1
  void $ Emulator.waitNSlots 1

  callEndpoint @"update" h1 test_1
  void $ Emulator.waitNSlots 1

  callEndpoint @"update" h1 test_2
  void $ Emulator.waitNSlots 1
  where
    getHandler :: ContractHandle (Last Handler) HandlerSchema Text -> EmulatorTrace Handler
    getHandler h = do
      l <- observableState h
      case l of
        Last Nothing -> Emulator.waitNSlots 1 >> getHandler h
        Last (Just handler) -> Extras.logInfo (show handler) >> return handler

    test_1 = HandlerDatum {state = True, exchangeRate = 123456}
    test_2 = HandlerDatum {state = False, exchangeRate = 123456}
