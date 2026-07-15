import FpLeanStudy

def main : IO Unit :=
  IO.println s!"Hello, {hello}!"

def a := #["lol", "cat"]

#eval a[3]?

class MyMonoid (α : Type) where
  mempty : α
  mappend : α → α → α

instance : MyMonoid Nat where
  mempty := 0
  mappend := fun x y => x + y


#eval MyMonoid.mappend 3 4

#eval String.intercalate " - " ["a", "b", "c"]


def andThen (opt: Option α) (next: α → Option β) : (Option β) :=
  match opt with
  | none => none
  | some x => next x

def firstThird (xs : List α) : Option (α × α) :=
  match xs[0]? with
  | none => none
  | some first =>
    match xs[2]? with
    | none => none
    | some third => some (first, third)


def firstThird' (xs : List α) : Option (α × α) :=
  andThen xs[0]? (fun first => andThen xs[2]? (fun third => some (first, third)))

infixl:55 "~~>" => andThen

def firstThird'2 (xs : List α) : Option (α × α) :=
  xs[0]? ~~> fun first =>
  xs[2]? ~~> fun third =>
    some (first, third)

#eval firstThird'2 [6,7,8,9]



namespace LogMonad

def isEven : Int → Bool :=
  fun i => (i % 2) == 0

-- def sumAndFindEvens : List Int → List Int × Int
--   | [] => ([], 0)
--   | x :: xs =>
--     let (evens, sum) := sumAndFindEvens xs
--     (if isEven x then x :: evens else evens, x + sum)

def sumAndFindEvens : List Int → List Int × Int
  | [] => ([], 0)
  | x :: xs =>
    let nowLog := if isEven x then [x] else []
    let nowSum := x
    let {fst := nextLog, snd := nextSum} := (sumAndFindEvens xs)

    (nowLog ++ nextLog, nowSum + nextSum)



#eval sumAndFindEvens [1,2,3,4,5,6,7,8,9]

structure WithLog (logged : Type) (α : Type) where
  log : List logged
  val : α

def andThen (result: WithLog α β) (next: β → WithLog α γ) : WithLog α γ :=
  let new := next result.val
  {
    log := result.log ++ new.log
    val := new.val
  }

def ok (x : β) : WithLog α β := {log := [], val := x}
def save (x : α) : WithLog α Unit := {log := [x], val := ()}

-- def andThen (result: WithLog α β) (next: β → WithLog α γ) : WithLog α γ :=
--   let {log := thisLog, val := thisVal} := result;
--   let {log := nextLog, val := nextVal} := next thisVal;
--   {
--     log := thisLog ++ nextLog
--     val := nextVal
--   }

infixl:55 " ~~> " => andThen


def sumAndFindEvens2 : List Int → WithLog Int Int
  | [] => ok 0
  | x :: xs =>
    (if isEven x then save x else ok ()) ~~> fun _ =>
    (sumAndFindEvens2 xs) ~~> fun value =>
    ok (x + value)

#eval sumAndFindEvens2 [1,2,3,4,5,6,7,8,9]

inductive BinTree (α : Type) where
  | leaf : BinTree α
  | branch : BinTree α -> α -> BinTree α -> BinTree α

-- 遍历并求和
def inorderSum : BinTree Int → WithLog Int Int
  | BinTree.leaf => ok 0
  | BinTree.branch l x r =>
    (inorderSum l) ~~> fun lsum =>
    save x ~~> fun _ =>
    (inorderSum r) ~~> fun rsum =>
    ok (lsum+x+rsum)

def t1: BinTree Int := BinTree.branch (BinTree.branch BinTree.leaf 1 BinTree.leaf) 8 (BinTree.branch BinTree.leaf 3 BinTree.leaf)

#eval inorderSum t1

end LogMonad
