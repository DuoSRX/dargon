module Types where

data Exp
  -- Variable
  = EVar String
  -- Literal
  | ELit Lit
  -- Function application
  | EApp Exp Exp
  -- Lambda
  | ELam String Exp
  -- Let binding
  | ELet String Exp Exp
  -- Binary operation, like +, - ...
  | EOp Binop Exp Exp
  -- if-this-then-else
  | EIf Exp Exp Exp
  -- Fix point. Used for recursion with letrec
  | EFix Exp
  -- Case expression (scrutinee and pairs of match pattern/arms)
  | ECase Exp [(Pattern, Exp)]
  deriving (Eq, Ord)

data Pattern
  = PVar String
  | PLit Lit
  -- | PWildcard
  deriving (Eq, Ord, Show)

-- foo = ECase (EVar "x") [ (PLit (LInt 0), ELit (LBool True))
--                        , (PLit (LInt 1), ELit (LBool False))
--                        , (PVar "y",      EVar "z")]

data Lit
  = LInt Integer
  | LBool Bool
  | LChar Char
  | LString String
  deriving (Eq, Ord)

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord)

data Type
  = TVar String
  | TInt
  | TBool
  | TChar
  | TString
  | TFun Type Type
  deriving (Eq, Ord)

-- List of bound type variables and a (maybe open) type
data Scheme = Scheme [String] Type

instance Show Type where
  show = prType

prType :: Type -> String
prType (TVar n) = n
prType TInt = "Int"
prType TBool = "Bool"
prType TChar = "Char"
prType TString = "String"
prType (TFun t s) = prParenType t ++ " -> " ++ prType s
 where prParenType t =
         case t of
           TFun _ _ -> "(" ++ prType t ++ ")"
           _        -> prType t

instance Show Exp where
  show = prExp

prExp :: Exp -> String
prExp (EVar name) = name
prExp (ELit lit)  = prLit lit
prExp (EFix e) = "fix " ++ prExp e
prExp (ELet x b body) = "let " ++ x ++ " = " ++ prExp b ++ " in " ++ prExp body
prExp (EApp e1 e2) = prExp e1 ++ " " ++ prParenExp e2
prExp (ELam n e) = "fn " ++ n ++ " -> " ++ prExp e
prExp (EOp op a b) = prExp a ++ " " ++ show op ++ " " ++ prExp b
prExp (EIf pred t e) = "if " ++ prExp pred ++ " then " ++ prExp t ++ " else " ++ prExp e
prExp (ECase s arms) = "case " ++ prExp s ++ " of " ++ show arms

prParenExp :: Exp -> String
prParenExp t = case t of
  ELet {} -> "(" ++ prExp t ++ ")"
  EApp {} -> "(" ++ prExp t ++ ")"
  ELam {} -> "(" ++ prExp t ++ ")"
  _       -> prExp t

instance Show Binop where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Eql = "=="

instance Show Lit where
  show = prLit

prLit :: Lit -> String
prLit (LInt i) = show i
prLit (LBool b) = if b then "True" else "False"
prLit (LChar c) = show c
prLit (LString s) = s

instance Show Scheme where
  show = prScheme

prScheme :: Scheme -> String
prScheme (Scheme [] t) = prType t
prScheme (Scheme vars t) = "∀" ++ unwords vars ++ ". " ++ prType t
