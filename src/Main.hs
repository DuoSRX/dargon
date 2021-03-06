{-# LANGUAGE FlexibleContexts #-}
module Lib (main) where

import Control.Monad.State.Strict
import Data.Maybe
import Data.Monoid

import System.Console.Repline
import System.Exit
import System.IO

import Interpreter
import Parser
import TypeInference

data InterpreterState = InterpreterState {
    types :: TypeEnv
  , terms :: TermEnv
  }

type Repl a = HaskelineT (StateT InterpreterState IO) a

emptyState :: InterpreterState
emptyState = InterpreterState emptyTypeEnv emptyTermEnv

cmd :: String -> Repl ()
cmd input = do
  state <- get
  case parseProgram input of
    Left err -> liftIO $ putStrLn $ "Parse error: " ++ show err
    Right program -> do
      typeContext <- gets types
      case inferProgram typeContext program of
        Left err -> liftIO $ putStrLn $ "Type error: " ++ show err
        Right newTypeContext -> do
          let newState = state { terms = foldl evalDeclaration (terms state) program
                               , types = newTypeContext <> typeContext }
          put newState
          case lookup "expr" program of
            Nothing -> return ()
            Just expr -> do
              let (val, _) = runEval (terms newState) "expr" expr
              let valType = fromMaybe (error "Impossible") (typeOf (types newState) "expr")
              liftIO $ putStrLn $ show val ++ " : " ++ show valType

evalDeclaration env (name, expr) = newTermsContext
  where (_, newTermsContext) = runEval env name expr

quit :: a -> Repl ()
quit _ = liftIO exitSuccess

showType :: [String] -> Repl ()
showType args = do
  typeContext <- gets types
  let func = unwords args
  case typeOf typeContext func of
    Just t -> liftIO $ putStrLn $ func ++ " : " ++ show t
    Nothing -> liftIO $ putStrLn "¯\\_(ツ)_/¯"

-- TODO: Add real completion
complete :: (MonadState InterpreterState m) => WordCompleter m
complete _ = return []

completer :: CompleterStyle (StateT InterpreterState IO)
completer = Prefix (wordCompleter complete) []

welcome :: Repl ()
welcome = liftIO $ putStrLn "Here be Dargons!"

repl :: IO ()
repl = evalStateT (evalRepl "> " cmd [("quit", quit), ("type", showType)] completer welcome) emptyState

main :: IO ()
main = do
  hSetEncoding stdout utf8
  repl
