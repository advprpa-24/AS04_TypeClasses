# Assignment 4 - Type Classes

## Basic classes
Given the following data type:

`data Person = MkPerson { nr:: Int, email :: String }`

**TODO:**\
Manually implement instances for the classes `Show`, `Eq` and `Ord`. Equality and ordering should depend solely on the `nr` attribute. Write your solution into [TypeClasses/Person.hs](./TypeClasses/Person.hs).


Execute `ghci TypeClasses/Person.hs` to load the code into ghci.


## Natural Numbers in Haskell

Natural numbers can be represented using the following type:

`data Nat = Z | S Nat`

**TODO:**\
Complete the functions and the `Num Nat` instance in the file [TypeClasses/Nat.hs](./TypeClasses/Nat.hs).

Execute `ghci TypeClasses/Nat.hs` to load the code into ghci.

Side note: This is not just a curiosity. Due to their recursive structure, natural numbers are tractable with inductive proofs. This will be covered towards the end of the semester. 


## Map Data Structure
In this part, you will implement a map data structure based on a sorted tree.
We will use this structure in the following exercises:

`data Map k v = Empty | Branch (Map k v) k v (Map k v)`

**TODO:**\
Complete the implementation in [Map/TreeMap.hs](./Map/TreeMap.hs). In the same file define a 
`Show`, `Semigroup` and a `Monoid` instance for your `Map` data type. Add more test cases to [Map/Tests.hs](./Map/Tests.hs).

Execute `cabal test map --test-show-details=direct` to run the tests.

Note: There are [much more efficient map data structures](https://hackage.haskell.org/package/containers-0.7/docs/Data-Map-Strict.html) available in Haskell.


## Map Reduce (application)
In this exercise, you will implement a local Map-Reduce to determine the frequency of words in text files. You already implemented a `Map` data structure to manage the data in the part before.

**TODO:**\
Understand the code in [MapReduce/Wordcount.hs](./MapReduce/Wordcount.hs) and complete the implementation of the `mapReduce` function.

Execute `ghci MapReduce/Wordcount.hs` to load the example into ghci.

Expected output:
```
ghci> main
count: 3588
top10:[("the",Sum {getSum = 1800}),("and",Sum {getSum = 908}),("to",Sum {getSum = 800}),("a",Sum {getSum = 682}),("of",Sum {getSum = 625}),("it",Sum {getSum = 578}),("she",Sum {getSum = 540}),("i",Sum {getSum = 531}),("you",Sum {getSum = 473}),("said",Sum {getSum = 453})]
it :: ()
```

Add more `.txt` files besides [alice.txt](./alice.txt) to test your implementation.

## Imp Language
In this last exercise you implement a small imperative language. It consists of expressions and commands. 

### Expressions and eval
Here is a GADT modeling expressions of the language.

```haskell
-- Type alias for variable names
type Name = String

-- Type alias for mappings from variable names to their values
-- E.g. fromList [("x", 2), ("y", 6)]
type State = M.Map Name Integer

-- Expr describes expressions
data Expr a where
    -- Boolean expressions
    BNot :: Expr Bool -> Expr Bool -- Boolean inverse (not)
    BAnd :: Expr Bool -> Expr Bool -> Expr Bool -- Boolean And (&&)

    -- Relational expressions
    REq :: Expr Integer -> Expr Integer -> Expr Bool -- Equality (==)
    RLt :: Expr Integer -> Expr Integer -> Expr Bool -- Less than (<)

    -- Arithmetic expressions
    AConst :: Integer -> Expr Integer -- Constant, e.g. `AConst 42`
    AVar   :: Name -> Expr Integer -- Variable, e.g. `Var "x"`
    APlus  :: Expr Integer -> Expr Integer -> Expr Integer -- Integer addition (+)
    AMinus :: Expr Integer -> Expr Integer -> Expr Integer -- Integer subtraction (-)
    AMul   :: Expr Integer -> Expr Integer -> Expr Integer -- Integer multiplication (*)
    ADiv   :: Expr Integer -> Expr Integer -> Expr Integer -- Integer division `div`
    AMod   :: Expr Integer -> Expr Integer -> Expr Integer -- Integer modulus `mod`
```

**TODO:**\
Implement the `eval` function:

`eval :: Expr a -> State -> a`

It takes an expression and a state (mapping variable names to integer values) and computes the result.
Use `M.lookup` to get the integer value for a variable. If `Nothing` is returned, terminate the program with an `error`.

Add more test cases to [Lang/Tests.hs](./Lang/Tests.hs).

### Commands and exec

The following type describes commands in our language. 

```haskell
-- Cmd describes the available commands
data Cmd = 
    CSkip -- No operation, has no effect
  | CAssign Name (Expr Integer) -- Assigns the result of evaluating (`Expr Integer`) to the variable `Name`
  | CIfThEl (Expr Bool) Cmd Cmd -- If then else, evaluates the condition and depending on the result executes the first Cmd (then branch) or the second (else branch)
  | CWhile (Expr Bool) Cmd -- While loop
  | CSeq [Cmd] -- Sequence of commands
```

Note: `CAssign` is the only command which updates the state.

**TODO:**\
Implement the `execute` function. 

`exec :: Cmd -> State -> State`

It takes a command and a state and yields the modified state. It makes use of the `eval` function.

Again, add more test cases to [Lang/Tests.hs](./Lang/Tests.hs).

Execute `cabal test imp --test-show-details=direct` to run the tests.
