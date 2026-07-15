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
