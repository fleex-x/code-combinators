import CodeCombinators
import Test.HUnit
import qualified Text.PrettyPrint as PP
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH (Lit(IntegerL))


cmpDocExp :: DocExp -> DocExp -> Bool
cmpDocExp (DocExp e1) (DocExp e2) = e1 noPrec == e2 noPrec 

docE :: String -> DocExp
docE str = DocExp $ \_ -> PP.text str

exps :: Test
exps = TestCase  $ do
            assertBool "small-exp-1" $ cmpDocExp (appE (appE func1 (intE 100)) (intE 20)) (docE "func1 100 20")
            assertBool "small-exp-2" $ cmpDocExp (appE (appE func1 (appE func1 (intE 100))) (intE 20)) (docE "func1 (func1 100) 20")
            assertBool "small-exp-3" $ cmpDocExp (appE (appE func1 (tupE [intE 100, intE 20, appE func1 (intE 30)])) (intE 20)) (docE "func1 (100, 20, func1 30) 20")
            assertBool "small-exp-4" $ cmpDocExp (appE (appE func1 (appE func1 $ listE [intE 100, intE 20, appE func1 (intE 30)])) (intE 20)) (docE "func1 (func1 [100, 20, func1 30]) 20")
            where func1 = varE $ mkName "func1"


cmpDocPat :: DocPat -> DocPat -> Bool
cmpDocPat (DocPat p1) (DocPat p2) = p1 noPrec == p2 noPrec 

docP :: String -> DocPat
docP str = DocPat $ \_ -> PP.text str

patterns :: Test 
patterns = TestCase $ do
           assertBool "patterns-1" $ cmpDocPat (cons [lit100, x, y, litA]) (docP "Cons 100 x y \"A\"")
           assertBool "patterns-2" $ cmpDocPat (cons [cons [x], x, y, litA]) (docP "Cons (Cons x) x y \"A\"")
           assertBool "patterns-3" $ cmpDocPat (cons [cons [cons [x], tupP [x, y], litA], lit100]) (docP "Cons (Cons (Cons x) (x, y) \"A\") 100")  
           where lit100 = litP (TH.IntegerL 100)
                 litA = litP (TH.StringL "A")
                 x = varP $ mkName "x"
                 y = varP $ mkName "y"
                 cons = conP $ mkName "Cons"

cmpDocType :: DocType -> DocType -> Bool
cmpDocType (DocType t1) (DocType t2) = t1 noPrec == t2 noPrec 

docT :: String -> DocType
docT str = DocType $ \_ -> PP.text str

types :: Test 
types = TestCase $ do
        assertBool "types-1" $ cmpDocType (appT arrow a) (docT "(->) a")
        assertBool "types-3" $ cmpDocType (appT c (appT (appT arrow a) b)) (docT "c ((->) a b)")
        where arrow = varT $ mkName "(->)"
              a = varT $ mkName "a"
              b = varT $ mkName "b"
              c = varT $ mkName "c"       


docD :: String -> DocDec 
docD str = DocDec $ PP.text str

funcDec :: Test
funcDec = TestCase $ assertEqual "func with 2 clauses" (funD fun [cl1, cl2]) (docD $ "fun a b c = c (a b)\n" ++
                                                                                     "fun Cons (Cons a c) b c 100 = c a (c b)")
        where a = varE $ mkName "a"
              b = varE $ mkName "b"
              c = varE $ mkName "c"

              ap = varP $ mkName "a"
              bp = varP $ mkName "b"
              cp = varP $ mkName "c"

              fun = mkName "fun"
              con = mkName "Cons"

              exp1 = appE c (appE a b)
              exp2 = (c `appE` a) `appE` (c `appE` b)

              pts1 = [ap, bp, cp]
              pts2 = [conP con [conP con [ap, cp], bp, cp], litP (TH.IntegerL 100)]
              cl1 = clause pts1 exp1 []
              cl2 = clause pts2 exp2 []

sigDec :: Test
sigDec = TestCase $ assertEqual "func signature" (sigD fun type_) (docD "fun :: (->) ((->) a b) c")
        where arrow = conT $ mkName "(->)"
              a = varT $ mkName "a"
              b = varT $ mkName "b"
              c = varT $ mkName "c"

              fun = mkName "fun"
              type_ = (arrow `appT` ((arrow `appT` a) `appT` b)) `appT` c


funWhereSection :: Test 
funWhereSection = TestCase $ assertEqual "func with where" (funD fun [cl]) (docD $ "fun a b c = c (a b)\n"                    ++
                                                                                   "        where\n"                          ++
                                                                                   "            nestedF1 :: t1\n"             ++
                                                                                   "            nestedF1 = mempty\n"          ++
                                                                                   "                         where\n"         ++
                                                                                   "                             mempty = 100"  )
            where a = varE $ mkName "a"
                  b = varE $ mkName "b"
                  c = varE $ mkName "c"

                  ap = varP $ mkName "a"
                  bp = varP $ mkName "b"
                  cp = varP $ mkName "c"

                  fun = mkName "fun"

                  exp = appE c (appE a b)
                  pts = [ap, bp, cp]
                  memptyD = funD (mkName "mempty") [clause [] (intE 100) []]
                  nestedD = [sigD (mkName "nestedF1") (conT $ mkName "t1"), funD (mkName "nestedF1") [clause [] (varE $ mkName "mempty") [memptyD]]]
                  cl = clause pts exp nestedD

tests :: Test
tests = TestList [exps, patterns, types, funcDec, sigDec, funWhereSection]

main :: IO Counts
main = runTestTT tests


nested :: [DocDec] -> PP.Doc
nested decs = PP.text "where" PP.$+$
              foldr (PP.$+$) PP.empty [PP.nest 4 dec | DocDec dec <- decs]