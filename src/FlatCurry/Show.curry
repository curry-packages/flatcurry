------------------------------------------------------------------------------
--- This library contains operations to transform FlatCurry programs
--- into string representations, either in a FlatCurry format or
--- in a Curry-like syntax.
---
--- This library contains
---
---   * show functions for a string representation of FlatCurry programs
---     (`showFlatProg`, `showFlatType`, `showFlatFunc`)
---   * functions for showing FlatCurry (type) expressions in (almost)
---     Curry syntax (`showCurryType`, `showCurryExpr`,...).
---
--- @author Michael Hanus
--- @version September 2024
------------------------------------------------------------------------------

module FlatCurry.Show
  ( showFlatProg, showFlatType, showFlatFunc
  , showCurryType, isClassContext
  , showCurryExpr, showCurryId, showCurryVar
  )
 where

import FlatCurry.Types
import Data.List
import Data.Char

--- Shows a FlatCurry program term as a string (with some pretty printing).
showFlatProg :: Prog -> String
showFlatProg (Prog modname imports types funcs ops) =
     " (Prog " ++ show modname
     ++ (if null imports then "\n  []" else
         "\n  [" ++ showFlatListElems show imports ++ "]")
     ++ (if null types then "\n  []" else
         "\n  [" ++ showFlatListElems showFlatType types ++ "\n ]")
     ++ "\n  [" ++ showFlatListElems showFlatFunc funcs ++ "\n  ]"
     ++ "\n " ++ showFlatList showFlatOp ops
     ++ "\n )\n"

showFlatVisibility :: Visibility -> String
showFlatVisibility Public  = " Public "
showFlatVisibility Private = " Private "

showFlatFixity :: Fixity -> String
showFlatFixity InfixOp = " InfixOp "
showFlatFixity InfixlOp = " InfixlOp "
showFlatFixity InfixrOp = " InfixrOp "

showFlatOp :: OpDecl -> String
showFlatOp (Op name fix prec) =
 "(Op " ++ show name ++ showFlatFixity fix ++ show prec ++ ")"

showFlatType :: TypeDecl -> String
showFlatType (Type name vis tpars consdecls) =
  "\n  (Type " ++ show name ++ showFlatVisibility vis
               ++ showFlatList show tpars
               ++ showFlatList showFlatCons consdecls ++ ")"
showFlatType (TypeSyn name vis tpars texp) =
  "\n  (TypeSyn " ++ show name ++ showFlatVisibility vis
                  ++ showFlatList show tpars
                  ++ showFlatTypeExpr texp ++ ")"
showFlatType (TypeNew name vis tpars consdecl) =
  "\n  (TypeNew " ++ show name ++ showFlatVisibility vis
                  ++ showFlatList show tpars
                  ++ showFlatNewCons consdecl ++ ")"

showFlatCons :: ConsDecl -> String
showFlatCons (Cons cname arity vis types) =
  "(Cons " ++ show cname ++ " " ++ show arity
           ++ showFlatVisibility vis
           ++ showFlatList showFlatTypeExpr types ++ ")"

showFlatNewCons :: NewConsDecl -> String
showFlatNewCons (NewCons cname vis texp) =
  "(NewCons " ++ show cname
              ++ showFlatVisibility vis
              ++ showFlatTypeExpr texp ++ ")"

showFlatFunc :: FuncDecl -> String
showFlatFunc (Func name arity vis ftype rl) =
  "\n  (Func " ++ show name ++ " " ++ show arity ++ " "
               ++ showFlatVisibility vis ++
  "\n        " ++ showFlatTypeExpr ftype ++
  "\n       " ++ showFlatRule rl ++ ")"

showFlatRule :: Rule -> String
showFlatRule (Rule params expr) =
  " (Rule " ++ showFlatList show params
            ++ showFlatExpr expr ++ ")"
showFlatRule (External name) =
  " (External " ++ show name ++ ")"

showFlatTypeExpr :: TypeExpr -> String
showFlatTypeExpr (FuncType t1 t2) =
  "(FuncType " ++ showFlatTypeExpr t1 ++ " " ++ showFlatTypeExpr t2 ++ ")"
showFlatTypeExpr (TCons tc ts) =
  "(TCons " ++ show tc
            ++ showFlatList showFlatTypeExpr ts ++ ")"
showFlatTypeExpr (TVar n) = "(TVar " ++ show n ++ ")"
showFlatTypeExpr (ForallType tvs te) =
  "(ForallType " ++ showFlatList show tvs ++ showFlatTypeExpr te ++ ")"

showFlatCombType :: CombType -> String
showFlatCombType FuncCall = "FuncCall"
showFlatCombType ConsCall = "ConsCall"
showFlatCombType (FuncPartCall n) = "(FuncPartCall " ++ show n ++ ")"
showFlatCombType (ConsPartCall n) = "(ConsPartCall " ++ show n ++ ")"

showFlatExpr :: Expr -> String
showFlatExpr (Var n) = "(Var " ++ show n ++ ")"
showFlatExpr (Lit l) = "(Lit " ++ showFlatLit l ++ ")"
showFlatExpr (Comb ctype cf es) =
  "(Comb " ++ showFlatCombType ctype ++ " "
           ++ show cf ++ showFlatList showFlatExpr es ++ ")"
showFlatExpr (Let bindings exp) =
  "(Let " ++ showFlatList showFlatBinding bindings ++ showFlatExpr exp ++ ")"
 where showFlatBinding (x,e) = "("++show x++","++showFlatExpr e++")"
showFlatExpr (Free xs e) =
  "(Free " ++ showFlatList show xs ++ showFlatExpr e ++ ")"
showFlatExpr (Or e1 e2) =
  "(Or " ++ showFlatExpr e1 ++ " " ++ showFlatExpr e2 ++ ")"
showFlatExpr (Case Rigid e bs) =
  "(Case Rigid " ++ showFlatExpr e ++ showFlatList showFlatBranch bs ++ ")"
showFlatExpr (Case Flex e bs) =
  "(Case Flex " ++ showFlatExpr e ++ showFlatList showFlatBranch bs ++ ")"
showFlatExpr (Typed e ty) =
  "(Typed " ++ showFlatExpr e ++ ' ' : showFlatTypeExpr ty ++ ")"

showFlatLit :: Literal -> String
showFlatLit (Intc   i) = "(Intc " ++ show i ++ ")"
showFlatLit (Floatc f) = "(Floatc " ++ show f ++ ")"
showFlatLit (Charc  c) = "(Charc " ++ show c ++ ")"

showFlatBranch :: BranchExpr -> String
showFlatBranch (Branch p e) = "(Branch " ++ showFlatPattern p
                                         ++ showFlatExpr e ++ ")"

showFlatPattern :: Pattern -> String
showFlatPattern (Pattern qn xs) =
      "(Pattern " ++ show qn
                  ++ showFlatList show xs ++ ")"
showFlatPattern (LPattern lit) = "(LPattern " ++ showFlatLit lit ++ ")"


-- format a finite list of elements:
showFlatList :: (a->String) -> [a] -> String
showFlatList format elems = " [" ++ showFlatListElems format elems ++ "] "

showFlatListElems :: (a->String) -> [a] -> String
showFlatListElems format elems = intercalate "," (map format elems)


------------------------------------------------------------------------------
--- Shows a FlatCurry type in Curry syntax.
---
--- @param trans - a translation function from qualified type names
---                to external type names
--- @param nested - True iff brackets must be written around complex types
--- @param texpr - the FlatCurry type expression to be formatted
--- @return the String representation of the formatted type expression

showCurryType :: (QName -> String) -> Bool -> TypeExpr -> String
showCurryType tf nested = showTypeWithClass []
 where
  showTypeWithClass cls texp = case texp of
    ForallType _ te -> showTypeWithClass cls te -- strip forall quantifiers
    FuncType t1 t2  -> maybe (showClassedType cls texp)
                             (\ (cn,cvs) ->
                                  showTypeWithClass (cls ++ [(cn,cvs)]) t2)
                             (isClassContext t1)
    _               -> showClassedType cls texp

  showClassedType cls texp
   | null cls
   = showCurryType_ tf nested texp
   | otherwise
   = showBracketsIf nested $
       showBracketsIf (length cls > 1)
         (intercalate ", "
            (map (\ (cn,cvs) -> unwords $
                      cn : map (\cv -> showCurryType_ tf True cv) cvs)
                 cls)) ++
         " => " ++ showCurryType_ tf False texp

--- Tests whether a FlatCurry type is a class context.
--- If it is the case, return the class name and the type parameters
--- of the class context.
isClassContext :: TypeExpr -> Maybe (String,[TypeExpr])
isClassContext texp = case texp of
  TCons (_,tc) ts -> checkDictCons tc ts
  -- a class context might be represented as function `() -> Dict`:
  FuncType (TCons unit []) (TCons (_,tc) ts) | unit == ("Prelude","()")
                   -> checkDictCons tc ts
  _                -> Nothing
 where
  checkDictCons tc ts | take 6 tc == "_Dict#" = Just (drop 6 tc, ts)
                      | otherwise             = Nothing

------------------------------

showCurryType_ :: (QName -> String) -> Bool -> TypeExpr -> String
showCurryType_ _ _ (TVar i) = if i<5 then [chr (97+i)] else 't':show i
showCurryType_ tf nested (FuncType t1 t2) =
  showBracketsIf nested
    (showCurryType_ tf (isFuncType t1) t1 ++ " -> " ++
     showCurryType_ tf False t2)
showCurryType_ tf nested (TCons tc ts)
 | null ts = tf tc
 | tc==("Prelude","[]") && (head ts == TCons ("Prelude","Char") [])
   = "String"
 | tc==("Prelude","[]")
  = "[" ++ showCurryType_ tf False (head ts) ++ "]" -- list type
 | take 2 (snd tc) == "(,"                          -- tuple type
  = "(" ++ intercalate "," (map (showCurryType_ tf False) ts) ++ ")"
 | otherwise
  = showBracketsIf nested
    (tf tc ++ concatMap (\t->' ':showCurryType_ tf True t) ts)
showCurryType_ tf nested (ForallType tvs te) =
  showBracketsIf nested
    (unwords ("forall" : map (showCurryType_ tf False . TVar . fst) tvs) ++ " . " ++
     showCurryType_ tf False te)

isFuncType :: TypeExpr -> Bool
isFuncType (TVar _)          = False
isFuncType (FuncType _ _)    = True
isFuncType (TCons _ _)       = False
isFuncType (ForallType _ te) = isFuncType te


------------------------------------------------------------------------------
--- Shows a FlatCurry expressions in (almost) Curry syntax.
---
--- @param trans - a translation function from qualified functions names
---                to external function names
--- @param nested - True iff brackets must be written around complex terms
--- @param indent - the indentation used in  case expressions and if-then-else
--- @param expr - the FlatCurry expression to be formatted
--- @return the String representation of the formatted expression

showCurryExpr :: (QName -> String) -> Bool -> Int -> Expr -> String

showCurryExpr _ _ _ (Var n) = showCurryVar n

showCurryExpr _ _ _ (Lit l) = showCurryLit l

showCurryExpr tf _ _ (Comb _ cf []) = showCurryId (tf cf)
showCurryExpr tf nested b (Comb _ cf [e]) =
  showBracketsIf nested (showCurryId (tf cf) ++ " "
                            ++ showCurryExpr tf True b e)
showCurryExpr tf nested b (Comb ct cf [e1,e2])
 | cf==("Prelude","apply")
  = showBracketsIf nested
       (showCurryExpr tf True b e1 ++ " " ++ showCurryExpr tf True b e2)
 | isAlpha (head (snd cf))
  = showBracketsIf nested
    (tf cf ++" "++ showCurryElems (showCurryExpr tf True b) [e1,e2])
 | isFiniteList (Comb ct cf [e1,e2])
  = if isStringConstant (Comb ct cf [e1,e2])
    then "\"" ++ showCurryStringConstant (Comb ct cf [e1,e2]) ++ "\""
    else "[" ++
         intercalate "," (showCurryFiniteList tf b (Comb ct cf [e1,e2]))
         ++ "]"
 | snd cf == "(,)" -- pair constructor?
  = "(" ++ showCurryExpr tf False b e1 ++ "," ++
           showCurryExpr tf False b e2 ++ ")"
 | otherwise
  = showBracketsIf nested
              (showCurryExpr tf True b e1 ++ " " ++ tf cf ++ " " ++
               showCurryExpr tf True b e2 )
showCurryExpr tf nested b (Comb _ cf (e1:e2:e3:es))
 | cf==("Prelude","if_then_else") && null es
  = showBracketsIf nested
        ("\n" ++
         sceBlanks b ++ " if "   ++ showCurryExpr tf False (b+2) e1 ++ "\n" ++
         sceBlanks b ++ " then " ++ showCurryExpr tf False (b+2) e2 ++ "\n" ++
         sceBlanks b ++ " else " ++ showCurryExpr tf False (b+2) e3)
 | take 2 (snd cf) == "(,"  -- tuple constructor?
  = "(" ++
    intercalate "," (map (showCurryExpr tf False b) (e1:e2:e3:es))
        ++ ")"
 | otherwise
  = showBracketsIf nested
       (showCurryId (tf cf) ++ " "
        ++ showCurryElems (showCurryExpr tf True b) (e1:e2:e3:es))

showCurryExpr tf nested b (Let bindings exp) =
  showBracketsIf nested
    ("\n" ++ sceBlanks b ++ "let " ++
     intercalate ("\n    " ++ sceBlanks b)
       (map (\ (x,e) -> showCurryVar x ++ " = " ++
                         showCurryExpr tf False (b+4) e) bindings) ++
     ("\n" ++ sceBlanks b ++ " in ") ++ showCurryExpr tf False (b+4) exp)

showCurryExpr tf nested b (Free [] e) = showCurryExpr tf nested b e

showCurryExpr tf nested b (Free (x:xs) e) =
  showBracketsIf nested
    ("let " ++ intercalate "," (map showCurryVar (x:xs)) ++
     " free in " ++ showCurryExpr tf False b e)

showCurryExpr tf nested b (Or e1 e2) =
  showBracketsIf nested
    (showCurryExpr tf True b e1 ++ " ? " ++ showCurryExpr tf True b e2)

showCurryExpr tf nested b (Case ctype e cs) =
  showBracketsIf nested
    ((case ctype of Rigid -> "case "
                    Flex  -> "fcase ") ++
     showCurryExpr tf True b e ++ " of\n " ++
     showCurryElems (showCurryCase tf (b+2)) cs ++ sceBlanks b)

showCurryExpr tf nested b (Typed e ty) =
  showBracketsIf nested
    (showCurryExpr tf True b e ++ " :: " ++ showCurryType tf False ty)

showCurryVar :: Show a => a -> String
showCurryVar i = "v" ++ show i

--- Shows an identifier in Curry form. Thus, operators are enclosed in brackets.
showCurryId :: String -> String
showCurryId name | isAlpha (head name) = name
                 | name == "[]"        = name
                 | otherwise           = ('(':name)++")"

showCurryLit :: Literal -> String
showCurryLit (Intc   i) = show i
showCurryLit (Floatc f) = show f
showCurryLit (Charc  c) = show c

showCurryCase :: (QName -> String) -> Int -> BranchExpr -> String
showCurryCase tf b (Branch (Pattern l vs) e) =
  sceBlanks b ++ showPattern (tf l) vs
              ++ " -> " ++ showCurryExpr tf False b e ++ "\n"
 where
   showPattern c [] = c
   showPattern c [x] = c ++ " " ++ showCurryVar x
   showPattern c [x1,x2] =
     if isAlpha (head c)
     then c ++ " " ++ showCurryVar x1 ++ " " ++ showCurryVar x2
     else if c=="(,)" -- pair constructor?
          then "(" ++ showCurryVar x1 ++ "," ++ showCurryVar x2 ++ ")"
          else showCurryVar x1 ++ " " ++ c ++ " " ++ showCurryVar x2
   showPattern c (x1:x2:x3:xs) =
     if take 2 c == "(,"  -- tuple constructor?
     then "(" ++ intercalate "," (map showCurryVar (x1:x2:x3:xs)) ++ ")"
     else c ++ " " ++ showCurryElems showCurryVar (x1:x2:x3:xs)

showCurryCase tf b (Branch (LPattern l) e) =
  sceBlanks b ++ showCurryLit l ++ " "
              ++ " -> " ++ showCurryExpr tf False b e ++ "\n"

showCurryFiniteList :: (QName -> String) -> Int -> Expr -> [String]
showCurryFiniteList tf b exp = case exp of
  Comb _ ("Prelude","[]") []     -> []
  Comb _ ("Prelude",":") [e1,e2] ->
    showCurryExpr tf False b e1 : showCurryFiniteList tf b e2
  _ -> error "Internal error in FlatCurry.Show.showCurryFiniteList"

-- show a string constant
showCurryStringConstant :: Expr -> String
showCurryStringConstant exp = case exp of
  Comb _ ("Prelude","[]") []                 -> []
  Comb _ ("Prelude",":") [Lit (Charc c), e2] ->
    showChar c ++ showCurryStringConstant e2
  _ -> error "Internal error in FlatCurry.Show.showCurryStringConstant"
 where
  showChar c
    | c=='"'  = "\\\""
    | c=='\'' = "\\\'"
    | c=='\n' = "\\n"
    | o < 32 || o > 126
    = ['\\', chr (o `div` 100 + 48), chr (((o `mod` 100) `div` 10 + 48)),
             chr(o `mod` 10 + 48)]
    | otherwise = [c]
   where
     o = ord c

showCurryElems :: (a -> String) -> [a] -> String
showCurryElems format elems = intercalate " " (map format elems)

showBracketsIf :: Bool -> String -> String
showBracketsIf nested s = if nested then '(' : s ++ ")" else s

sceBlanks :: Int -> String
sceBlanks b = take b (repeat ' ')

-- Is the expression a finite list (with an empty list at the end)?
isFiniteList :: Expr -> Bool
isFiniteList (Var _) = False
isFiniteList (Lit _) = False
isFiniteList (Comb _ name args)
  | name==("Prelude","[]") && null args = True
  | name==("Prelude",":") && length args == 2 = isFiniteList (args!!1)
  | otherwise = False
isFiniteList (Let _ _) = False
isFiniteList (Free _ _) = False
isFiniteList (Or _ _) = False
isFiniteList (Case _ _ _) = False
isFiniteList (Typed e _) = isFiniteList e

-- Is the expression a string constant?
isStringConstant :: Expr -> Bool
isStringConstant e = case e of
  Comb _ name args -> (name==("Prelude","[]") && null args) ||
                      (name==("Prelude",":") && length args == 2 &&
                       isCharConstant (head args) && isStringConstant (args!!1))
  _                -> False

-- Is the expression a character constant?
isCharConstant :: Expr -> Bool
isCharConstant e = case e of
  Lit (Charc _) -> True
  _             -> False


------------------------------------------------------------------------------
