module Interpreter where

import Control.Monad.Identity
import qualified Data.Map as Map

import Types

data Value
  = VInt Integer
  | VBool Bool
  | VChar Char
  | VString String
  | VClosure String Exp TermEnv

instance Show Value where
  show (VInt i) = show i
  show (VBool b) = show b
  show (VChar c) = show c
  show (VString s) = s
  show x = show x

type TermEnv = Map.Map String Value
type Interpreter t = Identity t

emptyTermEnv :: TermEnv
emptyTermEnv = Map.empty

eval :: TermEnv -> Exp -> Interpreter Value
eval env expr = case expr of
  ELit (LInt x)  -> return $ VInt x
  ELit (LBool x) -> return $ VBool x
  ELit (LChar x) -> return $ VChar x
  ELit (LString x) -> return $ VString x

  EVar x -> do
    let Just v = Map.lookup x env
    return v

  EOp op a b -> do
    VInt a' <- eval env a
    VInt b' <- eval env b
    return $ binop op a' b'

  EIf pred t e -> do
    VBool result <- eval env pred
    if result
      then eval env t
      else eval env e

  ELam x body ->
    return $ VClosure x body env

  -- Awyiss fixed point combinator
  EFix e -> eval env $ EApp e (EFix e)

  ELet x e body -> do
    e' <- eval env e
    let env' = Map.insert x e' env
    eval env' body

  EApp fun arg -> do
    VClosure x body closure <- eval env fun
    argv <- eval env arg
    let env' = Map.insert x argv closure
    eval env' body

runEval :: TermEnv -> String -> Exp -> (Value, TermEnv)
runEval env name expr = (result, Map.insert name result env)
  where result = runIdentity (eval env expr)

binop Add a b = VInt $ a + b
binop Sub a b = VInt $ a - b
binop Mul a b = VInt $ a * b
binop Eql a b = VBool $ a == b
