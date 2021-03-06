{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module CodeCombinators where

import qualified Text.PrettyPrint as PP
import Language.Haskell.TH(Lit(..))

class CodeGen exp type_ name clause dec pat | exp    -> type_ name clause dec pat,
                                              type_  -> exp  name  clause dec pat,
                                              name   -> exp  type_ clause dec pat,
                                              clause -> exp  type_ name dec pat,
                                              dec    -> exp  type_ name clause pat,
                                              pat    -> exp  type_ name clause dec
                                              where
    mkName  :: String -> name
    intE    :: Int -> exp

    conE    :: name -> exp
    varE    :: name -> exp
    appE    :: exp -> exp -> exp

    tupE    :: [exp] -> exp
    listE   :: [exp] -> exp

    conT    :: name -> type_
    varT    :: name -> type_
    appT    :: type_ -> type_ -> type_

    litP    :: Lit -> pat
    varP    :: name -> pat
    tupP    :: [pat] -> pat
    conP    :: name -> [pat] -> pat

    clause :: [pat] -> exp -> [dec] -> clause

    sigD    :: name -> type_    -> dec
    funD    :: name -> [clause] -> dec

newtype Prec = Prec Int
  deriving (Eq, Ord, Show, Bounded)

atomPrec, appPrec, noPrec :: Prec
atomPrec = Prec maxBound
appPrec  = Prec 10
noPrec   = Prec (-1)


newtype DocExp    = DocExp    (Prec -> PP.Doc)
newtype DocType   = DocType   (Prec -> PP.Doc)
newtype DocPat    = DocPat    (Prec -> PP.Doc)

newtype DocName   = DocName   PP.Doc
    deriving (Eq, Show)
newtype DocClause = DocClause PP.Doc
    deriving (Eq, Show)
newtype DocDec    = DocDec    PP.Doc
    deriving (Eq, Show)

instance CodeGen DocExp DocType DocName DocClause DocDec DocPat where
    mkName :: String -> DocName
    mkName name = DocName $ PP.text name

    intE :: Int -> DocExp
    intE num = DocExp (\_ -> parensIf (num < 0) (PP.int num))

    conE :: DocName -> DocExp
    conE (DocName name) = DocExp $ \_ -> name

    varE :: DocName -> DocExp
    varE (DocName name) = DocExp $ \_ -> name

    appE :: DocExp -> DocExp -> DocExp
    appE (DocExp e1) (DocExp e2) = DocExp $ \p -> parensIf (p > appPrec) $
                                                    PP.sep [e1 appPrec, e2 atomPrec]


    tupE :: [DocExp] -> DocExp
    tupE ds  = DocExp $ \_ -> PP.parens $ PP.sep $ PP.punctuate PP.comma $
                                [d noPrec | DocExp d <- ds]

    listE :: [DocExp] -> DocExp
    listE ds = DocExp $ \_ -> PP.brackets $ PP.sep $ PP.punctuate PP.comma $
                                [d noPrec | DocExp d <- ds]

    conT :: DocName -> DocType
    conT (DocName name) = DocType $ \_ -> name

    varT :: DocName -> DocType
    varT (DocName name) = DocType $ \_ -> name

    appT :: DocType -> DocType -> DocType
    appT (DocType t1) (DocType t2) = DocType $ \p -> parensIf (p > appPrec) $
                                                    PP.sep [t1 appPrec, t2 atomPrec]

    litP :: Lit -> DocPat
    litP (CharL c)    = DocPat $ \_ -> PP.quotes $ PP.text [c]
    litP (StringL s)  = DocPat $ \_ -> PP.doubleQuotes $ PP.text s
    litP (IntegerL n) = DocPat $ \_ -> parensIf (n < 0) $ PP.text $ show n

    varP :: DocName -> DocPat
    varP (DocName name)    = DocPat $ \_ -> name

    tupP :: [DocPat] -> DocPat
    tupP ps = DocPat $ \_ ->
              PP.parens $ PP.sep $ PP.punctuate PP.comma [p noPrec | DocPat p <- ps]

    conP :: DocName -> [DocPat] -> DocPat
    conP (DocName name) ps = DocPat $ \p -> parensIf (p > appPrec) $
                                            name PP.<+> PP.sep [p atomPrec | DocPat p <- ps]

    clause :: [DocPat] -> DocExp -> [DocDec] -> DocClause
    clause ps (DocExp exp) decs = DocClause $ (PP.sep [p noPrec | DocPat p <- ps] PP.<+>
                                               PP.text "=" PP.<+> exp noPrec) PP.$+$ PP.nest 4 whereSection
                                               where whereSection = case decs of
                                                                    [] -> PP.empty
                                                                    _  -> PP.text "where" PP.$+$
                                                                          foldr (PP.$+$) PP.empty [PP.nest 4 dec | DocDec dec <- decs]

    sigD :: DocName -> DocType -> DocDec
    sigD (DocName name) (DocType type_)  = DocDec $ name PP.<+> PP.text "::" PP.<+> type_ noPrec

    funD :: DocName -> [DocClause] -> DocDec
    funD (DocName name) cls = DocDec $ foldr1 (PP.$+$) [name PP.<+> cl | DocClause cl <- cls]


fromTextDetails :: PP.TextDetails -> ShowS
fromTextDetails td =
  case td of
    PP.Chr c -> (c:)
    PP.Str str -> (str++)
    PP.PStr str -> (str++)

renderDocDecs :: [[DocDec]] -> ShowS
renderDocDecs dss =
  PP.fullRender PP.PageMode 80 1.5 (\td s -> fromTextDetails td . s) id d
  where
    d = PP.vcat (map renderGroup dss)
    renderGroup ds = PP.vcat [ d1 | DocDec d1 <- ds ] PP.$+$ PP.text ""

parensIf :: Bool -> PP.Doc -> PP.Doc
parensIf True = PP.parens
parensIf False = id

