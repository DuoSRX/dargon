module Interpreter where

import Control.Monad.Identity
import Data.List (intercalate)
import qualified Data.Map as Map

import Types

data Value
  = VInt Integer
  | VBool Bool
  | VChar Char
  | VTup [Value]
  | VString String
  | VClosure String Exp TermEnv
  deriving (Eq)

instance Show Value where
  show (VInt i) = show i
  show (VBool b) = show b
  show (VChar c) = show c
  show (VString s) = s
  show (VTup xs) = "{" ++ (intercalate ", " (map show xs)) ++ "}"
  show x = show x

type TermEnv = Map.Map String Value
type Interpreter t = Identity t

emptyTermEnv :: TermEnv
emptyTermEnv = Map.empty

match :: TermEnv -> Pattern -> Value -> Bool
match env p v = let val = evalPattern env p
                in val == v

evalPattern :: TermEnv -> Pattern -> Value
evalPattern env pat = case pat of
  PVar v -> runIdentity $ eval env (EVar v)
  PLit l -> runIdentity $ eval env (ELit l)

eval :: TermEnv -> Exp -> Interpreter Value
eval env expr = case expr of
  ELit (LInt x)  -> return $ VInt x
  ELit (LBool x) -> return $ VBool x
  ELit (LChar x) -> return $ VChar x
  ELit (LString x) -> return $ VString x
  ELit (LTup xs) -> do
    values <- mapM (eval env) xs
    return $ VTup values

  ECase scrutinee arms -> do
    scrutinee' <- eval env scrutinee
    let res = filter fst $ map (\(pat, arm) -> (match env pat scrutinee', arm)) arms
    case res of
      (res:_) -> eval env (snd res)
      _ -> error "No matching arms"

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
