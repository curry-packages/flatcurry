--- Some tests for library FlatCurry.Goodies.
---
--- To run all tests automatically by the currycheck tool, use the command:
--- `curry-check TestFlatCurryGoodies`
--- 
--- @author Sebastian Fischer

import FlatCurry.Types
import FlatCurry.Files
import FlatCurry.Goodies
import Test.Prop

testIdentityTransformation = identity `returns` True

identity = do
  prog <- readFlatCurry "TestFlatCurryGoodies"
  return (prog == idProg prog)

idProg = trProg prog
 where
  prog name imps types funcs ops
    = Prog name imps (map idType types) (map idFunc funcs) (map idOp ops)

idType = trType typ typesyn typenew
 where
  typ name vis params cs      = Type name vis params (map idCons cs)
  typesyn name vis params syn = TypeSyn name vis params (idTypeExpr syn)
  typenew name vis params ncd = TypeNew name vis params (idNewCons ncd)

idCons = trCons cons
 where
  cons name arity vis args = Cons name arity vis (map idTypeExpr args)

idNewCons = trNewCons cons
 where
  cons name vis te = NewCons name vis (idTypeExpr te)

idTypeExpr = trTypeExpr TVar TCons FuncType ForallType

idFunc = trFunc func
 where
  func name arity vis t rule = Func name arity vis (idTypeExpr t) (idRule rule)

idRule = trRule rule External
 where
  rule args exp = Rule args (idExpr exp)

idExpr = trExpr Var Lit Comb Let Free Or Case Branch Typed

idOp = trOp Op


