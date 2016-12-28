{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TypeInference where

import Data.Maybe (fromMaybe)
import Control.Monad.Except
import Control.Monad.Trans.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Types

-- Mapping from type variables to types
type Subst = Map.Map String Type

class Types t where
  -- Determine the free type variables of a type.
  ftv :: t -> Set.Set String
  -- Applies a substition to a type.
  apply :: Subst -> t -> t

instance Types Type where
  ftv (TVar n) = Set.fromList [n]
  ftv TInt     = Set.empty
  ftv TBool    = Set.empty
  ftv TChar    = Set.empty
  ftv TString  = Set.empty
  ftv (TFun t1 t2) = ftv t1 `Set.union` ftv t2
  apply s (TVar n) = fromMaybe (TVar n) (Map.lookup n s)
  apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)
  apply _ t = t

instance Types Scheme where
  ftv (Scheme vars t) = ftv t `Set.difference` Set.fromList vars
  apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

instance Types a => Types [a] where
  ftv = foldr (Set.union . ftv) Set.empty
  apply s = map (apply s)

-- Type environment. Usually marked as Γ.
-- A mapping from names to type schemes.
newtype TypeEnv = TypeEnv (Map.Map String Scheme) deriving (Show, Monoid)

instance Types TypeEnv where
  ftv (TypeEnv env) = ftv (Map.elems env)
  apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv Map.empty

-- Extension: Γ∖x={y:σ|y:σ∈Γ,x≠y}
extend :: TypeEnv -> (String, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

-- Removes the binding from x from Γ. Usually called Γ\x
-- Γ,x:τ=(Γ∖x)∪{x:τ}
remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv $ Map.delete var env

typeOf :: TypeEnv -> String -> Maybe Scheme
typeOf (TypeEnv env) name = Map.lookup name env

-- Abstracts a type over all type varaibles which are free in the type
-- but not free in the given type environment.
generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
  where vars = Set.toList (ftv t `Set.difference` ftv env)

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- Type Inference stuff
data TIEnv = TIEnv
data TIState = TIState { tiSupply :: Int } deriving (Show)

data TypeError =
    CantUnify Type Type
  | OccursCheck String Type
  | OutOfScope String

instance Show TypeError where
  show (CantUnify t1 t2) = "Can't unify types: " ++ show t1 ++ " and " ++ show t2
  show (OccursCheck u t) = "Infinite type detected: " ++ u ++ " = " ++ show t
  show (OutOfScope v) = "Not in scope: " ++ v

-- This is kinda weird but that'll do for now.
type TI a = ExceptT TypeError (State TIState) a

-- Generate a type variable using letters.
-- Goes from a to z, then aa, ab and so on.
newTyVar :: TI Type
newTyVar = do
  s <- lift get
  lift $ put s { tiSupply = tiSupply s + 1 }
  return (TVar (letters !! tiSupply s))
  where letters = [1..] >>= flip replicateM ['a'..'z']

-- Convert a type scheme into a concrete type.
-- Replace the variables with fresh variables.
instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
  nvars <- replicateM (length vars) newTyVar
  let s = Map.fromList (zip vars nvars)
  return $ apply s t

-- Construct a substition from both types to the same type
unify :: Type -> Type -> TI Subst
unify (TFun l r) (TFun l' r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return $ s1 `composeSubst` s2
unify (TVar u) t = varBind u t
unify t (TVar u) = varBind u t
-- unify TBool TBool     = return nullSubst
-- unify TInt TInt       = return nullSubst
-- unify TChar TChar     = return nullSubst
-- unify TString TString = return nullSubst
unify t1 t2 | t1 == t2 = return nullSubst
unify t1 t2 = throwError $ CantUnify t1 t2

-- unifyMany :: [Type] -> [Type] -> TI (Subst, Type)
-- unifyMany [] t = return (nullSubst, t)
-- unifyMany t [] = return (nullSubst, t)
-- unifyMany (t1:t1s) (t2:t2s) = do
--   (s1, t1x) <- unify t1 t2
--   (s2, t2x) <- unifyMany (apply s1 t1s) (apply s1 t2s)
--   return (s2 `composeSubst` s1, t1x <> t2x)

-- unifyMany :: [Type] -> TI Subst
-- unifyMany [] = return nullSubst
-- unifyMany ts = do
--   foldr1 (\subst t -> ) ts

-- Construct a substitution from a name to a type unless
-- that name is already free in that type.
varBind :: String -> Type -> TI Subst
varBind u t
  | t == TVar u = return nullSubst
  | u `Set.member` ftv t = throwError $ OccursCheck u t
  | otherwise = return (Map.singleton u t)

tiLit :: Lit -> TI (Subst, Type)
tiLit (LInt _) = return (nullSubst, TInt)
tiLit (LBool _) = return (nullSubst, TBool)
tiLit (LChar _) = return (nullSubst, TChar)
tiLit (LString _) = return (nullSubst, TString)

patternToExp :: Pattern -> Exp
patternToExp (PVar v) = EVar v
patternToExp (PLit l) = ELit l

-- Infers types for expressions
ti :: TypeEnv -> Exp -> TI (Subst, Type)
ti _ (ELit l) = tiLit l
ti (TypeEnv env) (EVar n) =
  case Map.lookup n env of
    Just sigma -> do
      t <- instantiate sigma
      return (nullSubst, t)
    Nothing -> throwError $ OutOfScope n
ti env (ELam n e) = do
  tv <- newTyVar
  let TypeEnv env' = remove env n
      env'' = TypeEnv (env' `Map.union` Map.singleton n (Scheme [] tv))
  (s1, t1) <- ti env'' e
  return (s1, TFun (apply s1 tv) t1)
ti env (EApp e1 e2) = do
  tv <- newTyVar
  (s1, t1) <- ti env e1
  (s2, t2) <- ti (apply s1 env) e2
  s3 <- unify (apply s2 t1) (TFun t2 tv)
  return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
  -- `catchError` \e -> throwError $ e ++ " in " ++ show exp
ti env (ELet x e1 e2) = do
  (s1, t1) <- ti env e1
  let TypeEnv env' = remove env x
      t' = generalize (apply s1 env) t1
      env'' = TypeEnv (Map.insert x t' env')
  (s2, t2) <- ti (apply s1 env'') e2
  return (s1 `composeSubst` s2, t2)
ti env (EOp op e1 e2) = do
  (s1, t1) <- ti env e1
  (s2, t2) <- ti env e2
  tv <- newTyVar
  s3 <- unify (TFun t1 (TFun t2 tv)) (ops Map.! op)
  return (s1 `composeSubst` s2 `composeSubst` s3, apply s3 tv)
ti env (EIf pred t e) = do
  (s1, t1) <- ti env pred
  (s2, t2) <- ti env t
  (s3, t3) <- ti env e
  s4 <- unify t1 TBool
  s5 <- unify t2 t3
  return (s5 `composeSubst` s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1, apply s5 t2)
ti env (EFix e) = do
  (s1, t1) <- ti env e
  tv <- newTyVar
  s2 <- unify (TFun tv tv) t1
  return (s2, apply s1 tv)
ti env (ECase scrutinee arms) = do
  -- This works but... I'm 100% sure this is buggy as hell. And horrible.
  (_s1, t1) <- ti env scrutinee
  conds <- mapM (ti env . patternToExp . fst) arms
  _ <- mapM (unify t1) (map snd conds)

  (s2, t2) <- ti env (snd $ head arms)
  exprs <- mapM (ti env . snd) arms
  matches <- mapM (unify t2) (map snd exprs)
  return (foldr1 composeSubst matches, apply s2 t2)

-- tiPat :: Pattern -> TI (Type)
-- tiPat (PVar _) = do
--   v <- newTyVar
--   return v
-- tiPat (PWildcard) = do
--   v <- newTyVar
--   return v
-- tiPat (PLit l) = do
--   (_, t) <- tiLit l
--   return t

ops :: Map.Map Binop Type
ops = Map.fromList [
    (Add, TFun TInt (TFun TInt TInt))
  , (Sub, TFun TInt (TFun TInt TInt))
  , (Mul, TFun TInt (TFun TInt TInt))
  , (Eql, TFun TInt (TFun TInt TBool))
  ]

runInference :: TI (Subst, Type) -> Either TypeError Scheme
runInference m = case evalState (runExceptT m) TIState { tiSupply = 0 } of
  Left err -> Left err
  Right (sub, ty) -> Right $ generalize emptyTypeEnv (apply sub ty)

inferExp :: TypeEnv -> Exp -> Either TypeError Scheme
inferExp env = runInference . ti env

inferProgram :: TypeEnv -> [(String, Exp)] -> Either TypeError TypeEnv
inferProgram env [] = Right env
inferProgram env ((name, ex):xs) = case inferExp env ex of
  Left err -> Left err
  Right ty -> inferProgram (extend env (name, ty)) xs

typeInference :: Map.Map String Scheme -> Exp -> TI Type
typeInference env e = do
  (s, t) <- ti (TypeEnv env) e
  return $ apply s t
