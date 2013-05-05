module Halberd.CollectNames
  ( collectUnboundNames
  ) where

import           Control.Monad
import           Data.Either
import           Data.Function
import           Data.Generics
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Language.Haskell.Exts.Annotated        (SrcSpan)
import           Language.Haskell.Exts.Annotated.Syntax
import           Language.Haskell.Modules

------------------------------------------------------------------------------
-- Collecting the (unbound) names
------------------------------------------------------------------------------

data NameSpace = TypeSpace | ValueSpace  -- DON'T CHANGE THE ORDER
    deriving (Eq, Ord, Show)

type NameSpacedQName = (NameSpace, QName (Scoped SrcSpan))


collectUnboundNames :: Module (Scoped SrcSpan)
                    -> ([QName (Scoped SrcSpan)], [QName (Scoped SrcSpan)])
collectUnboundNames module_ = partitionEithers $ do
    (nameSpace, qname) <- namesFromAST module_
    guard (qNameNotInScope qname)
    return $ case nameSpace of
        TypeSpace  -> Left qname
        ValueSpace -> Right qname
  where
    qNameNotInScope :: QName (Scoped SrcSpan) -> Bool
    qNameNotInScope qname = case ann qname of
        ScopeError _ (ENotInScope _) -> True
        _                            -> False

    namesFromAST = everything (++) $
        mkQ [] namesFromAsst
        `extQ` namesFromInstHead
        `extQ` namesFromType
        `extQ` namesFromExp
        `extQ` namesFromFieldUpdate

namesFromAsst :: Asst l -> [(NameSpace, QName l)]
namesFromAsst x = case x of
    ClassA _ qn _   -> [(TypeSpace, qn)]
    InfixA _ _ qn _ -> [(TypeSpace, qn)]
    IParam _ _ _    -> []
    EqualP _ _ _    -> []

namesFromInstHead :: InstHead l -> [(NameSpace, QName l)]
namesFromInstHead x = case x of
    IHead _ qn _     -> [(TypeSpace, qn)]
    IHInfix _ _ qn _ -> [(TypeSpace, qn)]
    IHParen _ _      -> []

namesFromType :: Type l -> [(NameSpace, QName l)]
namesFromType x = case x of
    TyForall _ _ _ _ -> []
    TyFun _ _ _      -> []
    TyTuple _ _ _    -> []
    TyList _ _       -> []
    TyApp _ _ _      -> []
    TyVar _ _        -> []
    TyCon _ qn       -> [(TypeSpace, qn)]
    TyParen _ _      -> []
    TyInfix _ _ qn _ -> [(TypeSpace, qn)]
    TyKind _ _ _     -> []

namesFromExp :: Exp l -> [(NameSpace, QName l)]
namesFromExp x = case x of
    Var _ qn               -> [(ValueSpace, qn)]
    IPVar _ _              -> []
    Con _ qn               -> [(ValueSpace, qn)]
    Lit _ _                -> []
    InfixApp _ _ _ _       -> []
    App _ _ _              -> []
    NegApp _ _             -> []
    Lambda _ _ _           -> []
    Let _ _ _              -> []
    If _ _ _ _             -> []
    Case _ _ _             -> []
    Do _ _                 -> []
    MDo _ _                -> []
    Tuple _ _ _            -> []
    TupleSection _ _ _     -> []
    List _ _               -> []
    Paren _ _              -> []
    LeftSection _ _ _      -> []
    RightSection _ _ _     -> []
    RecConstr _ qn _       -> [(ValueSpace, qn)]
    RecUpdate _ _ _        -> []
    EnumFrom _ _           -> []
    EnumFromTo _ _ _       -> []
    EnumFromThen _ _ _     -> []
    EnumFromThenTo _ _ _ _ -> []
    ListComp _ _ _         -> []
    ParComp _ _ _          -> []
    ExpTypeSig _ _ _       -> []
    VarQuote _ qn          -> [(ValueSpace, qn)]
    TypQuote _ qn          -> [(TypeSpace, qn)]
    BracketExp _ _         -> []
    SpliceExp _ _          -> []
    QuasiQuote _ _ _       -> []
    XTag _ _ _ _ _         -> []
    XETag _ _ _ _          -> []
    XPcdata _ _            -> []
    XExpTag _ _            -> []
    XChildTag _ _          -> []
    CorePragma _ _ _       -> []
    SCCPragma _ _ _        -> []
    GenPragma _ _ _ _ _    -> []
    Proc _ _ _             -> []
    LeftArrApp _ _ _       -> []
    RightArrApp _ _ _      -> []
    LeftArrHighApp _ _ _   -> []
    RightArrHighApp _ _ _  -> []


namesFromFieldUpdate :: FieldUpdate l -> [(NameSpace, QName l)]
namesFromFieldUpdate x = case x of
    FieldUpdate _ qn _ -> [(ValueSpace, qn)]
    FieldPun _ _       -> []
    FieldWildcard _    -> []
