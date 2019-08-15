{-
   Trello

   This document describes the REST API of Trello as published by Trello.com.  - <a href='https://trello.com/docs/index.html' target='_blank'>Official Documentation</a>  - <a href='https://trello.com/docs/api' target='_blank'>The HTML pages that were scraped in order to generate this specification.</a>

   OpenAPI spec version: 2.0
   Trello API version: 1.0
   Generated by Swagger Codegen (https://github.com/swagger-api/swagger-codegen.git)
-}

{-|
Module : Trello.Logging
Katip Logging functions
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Trello.Logging where

import qualified Control.Exception.Safe     as E
import qualified Control.Monad.IO.Class     as P
import qualified Control.Monad.Trans.Reader as P
import qualified Data.Text                  as T
import qualified Lens.Micro                 as L
import qualified System.IO                  as IO

import           Data.Text                  (Text)
import           GHC.Exts                   (IsString (..))

import           Control.Lens
import qualified Katip                      as LG

-- * Type Aliases (for compatibility)

-- | Runs a Katip logging block with the Log environment
type LogExecWithContext = forall m. P.MonadIO m =>
                                    LogContext -> LogExec m

-- | A Katip logging block
type LogExec m = forall a. LG.KatipT m a -> m a

-- | A Katip Log environment
type LogContext = LG.LogEnv

-- | A Katip Log severity
type LogLevel = LG.Severity

-- * default logger

-- | the default log environment
initLogContext :: IO LogContext
initLogContext = LG.initLogEnv "Trello" "dev"

-- | Runs a Katip logging block with the Log environment
runDefaultLogExecWithContext :: LogExecWithContext
runDefaultLogExecWithContext = LG.runKatipT

-- * stdout logger

-- | Runs a Katip logging block with the Log environment
stdoutLoggingExec :: LogExecWithContext
stdoutLoggingExec = runDefaultLogExecWithContext

-- | A Katip Log environment which targets stdout
stdoutLoggingContext :: LogContext -> IO LogContext
stdoutLoggingContext cxt = do
  -- handleScribe <- LG.mkHandleScribe LG.ColorIfTerminal IO.stdout LG.InfoS LG.V2
  handleScribe <- LG.mkHandleScribe LG.ColorIfTerminal IO.stdout (\ item -> return $ item ^. LG.itemSeverity >= LG.InfoS) LG.V2
  LG.registerScribe "stdout" handleScribe LG.defaultScribeSettings cxt

-- * stderr logger

-- | Runs a Katip logging block with the Log environment
stderrLoggingExec :: LogExecWithContext
stderrLoggingExec = runDefaultLogExecWithContext

-- | A Katip Log environment which targets stderr
stderrLoggingContext :: LogContext -> IO LogContext
stderrLoggingContext cxt = do
--  handleScribe <- LG.mkHandleScribe LG.ColorIfTerminal IO.stderr LG.InfoS LG.V2
  handleScribe <- LG.mkHandleScribe LG.ColorIfTerminal IO.stderr (\ item -> return $ item ^. LG.itemSeverity >= LG.InfoS) LG.V2
  LG.registerScribe "stderr" handleScribe LG.defaultScribeSettings cxt

-- * Null logger

-- | Disables Katip logging
runNullLogExec :: LogExecWithContext
runNullLogExec le (LG.KatipT f) = P.runReaderT f (L.set LG.logEnvScribes mempty le)

-- * Log Msg

-- | Log a katip message
_log :: (Applicative m, LG.Katip m) => Text -> LogLevel -> Text -> m ()
_log src level msg = do
  LG.logMsg (fromString $ T.unpack src) level (LG.logStr msg)

-- * Log Exceptions

-- | re-throws exceptions after logging them
logExceptions
  :: (LG.Katip m, E.MonadCatch m, Applicative m)
  => Text -> m a -> m a
logExceptions src =
  E.handle
    (\(e :: E.SomeException) -> do
       _log src LG.ErrorS ((T.pack . show) e)
       E.throw e)

-- * Log Level

levelInfo :: LogLevel
levelInfo = LG.InfoS

levelError :: LogLevel
levelError = LG.ErrorS

levelDebug :: LogLevel
levelDebug = LG.DebugS

