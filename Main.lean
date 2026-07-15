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
