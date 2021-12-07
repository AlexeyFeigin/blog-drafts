# Incomplete Draft: Algorithm X in Haskell step by step

Now, don't panic. You don't necessarily need to know Haskell, as I will describe everything that's happening.

Suppose we have a set **U** = {1, 2, 3, 4, 5, 6, 7} and a set **S** of subsets of **U**:

**S** =

{

&nbsp;&nbsp;&nbsp;&nbsp;{1, 4, 7},

&nbsp;&nbsp;&nbsp;&nbsp;{1, 4},

&nbsp;&nbsp;&nbsp;&nbsp;{4, 5, 7},

&nbsp;&nbsp;&nbsp;&nbsp;{3, 5, 6},

&nbsp;&nbsp;&nbsp;&nbsp;{2, 3, 6, 7},

&nbsp;&nbsp;&nbsp;&nbsp;{2, 7}

}

Each element of **S** is some subset of **U**.

Suppose we start taking some arbitrary subsets of **S**, like this:

{

&nbsp;&nbsp;&nbsp;&nbsp;{1, 4, 7},

&nbsp;&nbsp;&nbsp;&nbsp;{1, 4},

}

{

&nbsp;&nbsp;&nbsp;&nbsp;{1, 4},

&nbsp;&nbsp;&nbsp;&nbsp;{3, 5, 6},

&nbsp;&nbsp;&nbsp;&nbsp;{2, 3, 6, 7},

}

...

How can we find a subset of **S** we will call **S\*** such that every element of **U** appears in exactly one element of **S\***.

This is an example of the _exact cover_ problem.

In our case the solution is this:

**S\*** =

{

&nbsp;&nbsp;&nbsp;&nbsp;{1, 4},

&nbsp;&nbsp;&nbsp;&nbsp;{3, 5, 6},

&nbsp;&nbsp;&nbsp;&nbsp;{2, 7}

}

Every element of **U** = {1, 2, 3, 4, 5, 6, 7} appears in exactly one element of **S\***.

An example of a non-solution is:

{

&nbsp;&nbsp;&nbsp;&nbsp;{2, 3, 6, 7},

}

Not all elements of **U** appear in an element of the set.

Another example of a non-solution is:

{

&nbsp;&nbsp;&nbsp;&nbsp;{1, 4, 7},

&nbsp;&nbsp;&nbsp;&nbsp;{3, 5, 6},

&nbsp;&nbsp;&nbsp;&nbsp;{2, 3, 6, 7},

&nbsp;&nbsp;&nbsp;&nbsp;{2, 7}

}

Some elements of **U** appear in more than one element of the set.


# Simple Algorithm X

Algorithm X is an algorithm developed by Donald Knuth that solves the exact cover problem. Let's implement Algorithm X in Haskell. We can later use it to solve Sudoku puzzles (in another blog post).

Here come some imports.

```Haskell
module AlgorithmX where

import Data.IntSet hiding (map, filter, foldl)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IntMap
import Data.List

import Text.Tabular
import Text.Tabular.AsciiArt
```

Don't panic.

Let's get some sets going.

```Haskell
set0 :: IntSet
set0 = fromList [1, 4, 7]
```

`set0` is an `IntSet` (a set of `Int`s). Internally, it is represented as an immutable tree.

We can interact with it in the `ghci` interpreter:

```Haskell
ghci> set0
fromList [1,4,7]

ghci> size set0
3

ghci> :t member
member :: Key -> IntSet -> Bool
```

The function `member` takes a `Key` and an `IntSet` and returns a `Bool` indicating whether the keys is a member of the set.

```Haskell
ghci> :i Key
type Key :: *
type Key = Int
        -- Defined in `Data.IntSet.Internal'
```

`Key` is a type synonym for `Int`.

Checking element membership:

```Haskell
ghci> set0
fromList [1,4,7]

ghci> member 1 set0
True

ghci> member 0 set0
False
```

Let's get some more sets going:

```Haskell
sets1 :: [IntSet]
sets1 = map fromList [ [1, 4, 7]
                     , [1, 4]
                     , [4, 5, 7]
                     , [3, 5, 6]
                     , [2, 3, 6, 7]
                     , [2 ,7]
                     ]
```

`sets1` is a list of `IntSet`s.

`sets1` is defined as a map that applies the function `fromList` to every element of the given list. (I know, Haskellers like to format lists like that... don't worry about it.)

I've prepared some printing functions earlier so we can print our data structures.

- `printList`
- `printScan`
- `instance Show SparseMatrix`

(Definitions in Appendix.)

Here's `sets1`:

```Haskell
ghci> printList "Set" sets1
+---++--------------------+
|   ||                Set |
+===++====================+
| 0 ||   fromList [1,4,7] |
| 1 ||     fromList [1,4] |
| 2 ||   fromList [4,5,7] |
| 3 ||   fromList [3,5,6] |
| 4 || fromList [2,3,6,7] |
| 5 ||     fromList [2,7] |
+---++--------------------+
```

In Algorithm X, we think of our sets as a matrix, like this:

|  | 1 | 2 | 3 | 4 | 5 | 6 | 7 |
|---|---|---|---|---|---|---|---|
**0** | 1 | 0 | 0 | 1 | 0 | 0 | 1
**1** | 1 | 0 | 0 | 1 | 0 | 0 | 0
**2** | 0 | 0 | 0 | 1 | 1 | 0 | 1
**3** | 0 | 0 | 1 | 0 | 1 | 1 | 0
**4** | 0 | 1 | 1 | 0 | 0 | 1 | 1
**5** | 0 | 1 | 0 | 0 | 0 | 0 | 1

The column labels are elements of **U**.
Each row represents an elements of **S**, having the value 1 denoting it contains that element of **U** or 0 denoting it does not contain it. So row **0** is the set `[1, 4, 7]`: you can see 1s in columns 1, 4 and 7 and 0s in the other columns.

In order to achieve exact cover we need to pick out the collection of rows such that exactly one 1 appears in every column of the matrix. We can use the matrix to visualise which sets overlap by looking at the columns. Additionally, if we search for a solution by crossing out rows we don't want to use, we can identify a dead end by finding a column with only 0s.

Now, this matrix will be sparse. Let's make a sparse matrix. Our sparse matrix will have rows but we can keep the `IntSet` representation of each row.

```Haskell
type Row = IntSet
```

`Row` is type synonym for `IntSet`.

```Haskell
ghci> set0 :: IntSet
fromList [1,4,7]

ghci> set0 :: Row   
fromList [1,4,7]
```

Our interpreter accepts that `set0` is an `IntSet` and also a `Row`.

Now we would like to have a collection of these rows.

```Haskell
type Rows = IntMap Row
```

`Rows` is an associative mapping from an `Int` to a `Row`. `IntMap` is internally represented as an immutable tree.

In order to make `Rows`, we need to give our `Row`s some indices.

```Haskell
ghci> printList "(Key, Row)" $ zip [0..] sets1
+---++------------------------+
|   ||             (Key, Row) |
+===++========================+
| 0 ||   (0,fromList [1,4,7]) |
| 1 ||     (1,fromList [1,4]) |
| 2 ||   (2,fromList [4,5,7]) |
| 3 ||   (3,fromList [3,5,6]) |
| 4 || (4,fromList [2,3,6,7]) |
| 5 ||     (5,fromList [2,7]) |
+---++------------------------+
```

Now we can put the rows into an `IntMap` of `Row`s.

```Haskell
rows1 = IntMap.fromList $ zip [0..] sets1
```

```Haskell
ghci> printList "(Key, Row)" $ IntMap.toList rows1                  
+---++------------------------+
|   ||             (Key, Row) |
+===++========================+
| 0 ||   (0,fromList [1,4,7]) |
| 1 ||     (1,fromList [1,4]) |
| 2 ||   (2,fromList [4,5,7]) |
| 3 ||   (3,fromList [3,5,6]) |
| 4 || (4,fromList [2,3,6,7]) |
| 5 ||     (5,fromList [2,7]) |
+---++------------------------+

ghci> rows1 ! 0
fromList [1,4,7]

ghci> rows1 ! 3
fromList [3,5,6]
```

We can query rows by their key (in _O(log n)_). 

We can remove a row:

```Haskell
ghci> printList "(Key, Row)" $ IntMap.toList $ IntMap.delete 3 rows1
+---++------------------------+
|   ||             (Key, Row) |
+===++========================+
| 0 ||   (0,fromList [1,4,7]) |
| 1 ||     (1,fromList [1,4]) |
| 2 ||   (2,fromList [4,5,7]) |
| 3 || (4,fromList [2,3,6,7]) |
| 4 ||     (5,fromList [2,7]) |
+---++------------------------+
```

Without affecting the original `rows1`:

```Haskell
ghci> printList "(Key, Row)" $ IntMap.toList rows1
+---++------------------------+
|   ||             (Key, Row) |
+===++========================+
| 0 ||   (0,fromList [1,4,7]) |
| 1 ||     (1,fromList [1,4]) |
| 2 ||   (2,fromList [4,5,7]) |
| 3 ||   (3,fromList [3,5,6]) |
| 4 || (4,fromList [2,3,6,7]) |
| 5 ||     (5,fromList [2,7]) |
+---++------------------------+
```

The `Rows` with the deleted row are a new data structure. Both the old and the new are immutable and the new re-uses much of the old structure under the hood.

This will be handy later when we will want to delete rows and columns from our sparse matrix and then backtrack.

We want to use something like `Rows` to represent our sparse matrix, but at the moment when we query for the existence of elements within our rows, we cannot tell whether the queried element is within the matrix or not.

```Haskell
ghci> rows1 ! 0
fromList [1,4,7]

ghci> member 1 $ rows1 ! 0
True
```

1 is a member of row 0.

```Haskell
ghci> member 2 $ rows1 ! 0
False
```

2 is a not a member of row 0.

```Haskell
ghci> member 10 $ rows1 ! 0
False
```

10 is not a member of row 0. But is it a member of the matrix?

So we need another piece of information to keep track of what is in our sparse matrix: which columns are in the matrix?

```Haskell
type ActiveCols = IntSet
```

`ActiveCols` is a type synonym for `IntSet`.

In our case, we would want to use the unions of our `sets1` to form the active columns.

```Haskell
ghci> printList "Set" sets1
+---++--------------------+
|   ||                Set |
+===++====================+
| 0 ||   fromList [1,4,7] |
| 1 ||     fromList [1,4] |
| 2 ||   fromList [4,5,7] |
| 3 ||   fromList [3,5,6] |
| 4 || fromList [2,3,6,7] |
| 5 ||     fromList [2,7] |
+---++--------------------+

ghci> unions sets1
fromList [1,2,3,4,5,6,7]
```

```Haskell
data SparseMatrix = SparseMatrix Rows ActiveCols
```

`SparseMatrix` is a new data type containing `Rows` and `ActiveCols`.

```Haskell
m1 = SparseMatrix rows1 (unions sets1)
```

```Haskell
ghci> print m1

+---++---+---+---+---+---+---+---+
|   || 1 | 2 | 3 | 4 | 5 | 6 | 7 |
+===++===+===+===+===+===+===+===+
| 0 || 1 | 0 | 0 | 1 | 0 | 0 | 1 |
| 1 || 1 | 0 | 0 | 1 | 0 | 0 | 0 |
| 2 || 0 | 0 | 0 | 1 | 1 | 0 | 1 |
| 3 || 0 | 0 | 1 | 0 | 1 | 1 | 0 |
| 4 || 0 | 1 | 1 | 0 | 0 | 1 | 1 |
| 5 || 0 | 1 | 0 | 0 | 0 | 0 | 1 |
+---++---+---+---+---+---+---+---+
```

The matrix prints out like this, but internally it is just `Rows` (i.e. `IntMap IntSet`) and `ActiveCols` (i.e. `IntSet`).

In order to search for an exact cover solution, we should pick a column and try to fill it. For the moment, column 1 seems as good as any. We have two choices of how to satisfy column 1: with row 0 or row 1. Row 0 seems as good as any.

Now we have to remove from the matrix all rows that conflict with row 0. Row 0 contains elements 1, 4 and 7. Any other rows that use these elements must be removed, otherwise we will have some elements of **U** in multiple elements of **S\***. In other words, we must only keep rows disjoint with row 0.

```Haskell
ghci> SparseMatrix (IntMap.filter ((rows1 ! 0) `disjoint`) rows1) (unions sets1)

+---++---+---+---+---+---+---+---+
|   || 1 | 2 | 3 | 4 | 5 | 6 | 7 |
+===++===+===+===+===+===+===+===+
| 3 || 0 | 0 | 1 | 0 | 1 | 1 | 0 |
+---++---+---+---+---+---+---+---+
```

Only row 3 is disjoint with row 0.

Another thing we need to do is remove columns from the matrix that we satisfied by picking row 0. We remove these from the `ActiveCols` of the matrix.

```Haskell
ghci> unions sets1
fromList [1,2,3,4,5,6,7]

ghci> unions sets1 `difference` (rows1 ! 0)
fromList [2,3,5,6]

ghci> SparseMatrix (IntMap.filter ((rows1 ! 0) `disjoint`) rows1) (unions sets1 `difference` (rows1 ! 0))

+---++---+---+---+---+
|   || 2 | 3 | 5 | 6 |
+===++===+===+===+===+
| 3 || 0 | 1 | 1 | 1 |
+---++---+---+---+---+
```

Unfortunately, we can see that column 2 cannot be satisfied by this remaining row. So row 0 cannot be part of the solution. We should go back and pick another row.

Let's go back and try row 1.

```Haskell
ghci> print m1

+---++---+---+---+---+---+---+---+
|   || 1 | 2 | 3 | 4 | 5 | 6 | 7 |
+===++===+===+===+===+===+===+===+
| 0 || 1 | 0 | 0 | 1 | 0 | 0 | 1 |
| 1 || 1 | 0 | 0 | 1 | 0 | 0 | 0 |
| 2 || 0 | 0 | 0 | 1 | 1 | 0 | 1 |
| 3 || 0 | 0 | 1 | 0 | 1 | 1 | 0 |
| 4 || 0 | 1 | 1 | 0 | 0 | 1 | 1 |
| 5 || 0 | 1 | 0 | 0 | 0 | 0 | 1 |
+---++---+---+---+---+---+---+---+

ghci> SparseMatrix (IntMap.filter ((rows1 ! 1) `disjoint`) rows1) (unions sets1 `difference` (rows1 ! 1))

+---++---+---+---+---+---+
|   || 2 | 3 | 5 | 6 | 7 |
+===++===+===+===+===+===+
| 3 || 0 | 1 | 1 | 1 | 0 |
| 4 || 1 | 1 | 0 | 1 | 1 |
| 5 || 1 | 0 | 0 | 0 | 1 |
+---++---+---+---+---+---+
```

This matrix represents the remaining options after having picked row 1.(Again, other rows conflicting with the choice and the filled columns have been removed.)

No columns have all 0s, so we could keep searching within this matrix in hopes of finding the complete solution.

After playing around like this, we are ready to look at Algorithm X and understand what it is trying to do.

From Wikipedia:

- If the matrix A has no columns, the current partial solution is a valid solution; terminate successfully.
    - Otherwise choose a column c (deterministically).
    - Choose a row r such that Ar, c = 1 (nondeterministically).
    - Include row r in the partial solution.
    - For each column j such that Ar, j = 1,
        - for each row i such that Ai, j = 1,
            - delete row i from matrix A.
        - delete column j from matrix A.
    - Repeat this algorithm recursively on the reduced matrix A.

"Deterministically" here means we have a procedure of picking some column right away, and "nondeteministically" means that the choice involves some kind of space search.

We have, in fact, just performed one iteration of Algorithm X manually. We chose a column, chose a row and reduced the matrix.

Instead of performing these steps through iteration

- ...
    - For each column j such that Ar, j = 1,
        - for each row i such that Ai, j = 1,
            - delete row i from matrix A.

we used equivalent the functionality to filter the rows by the criterion of being disjoint with the selected row (the `disjoint` operation runs in _O(n+m)_), and instead of

- ...
    - For each column j such that Ar, j = 1,
        - ...
        - delete column j from matrix A.

we used the `difference` between the active columns and the selected row (also _O(n+m)_). (Keep in mind our operations do not mutate the original data structure and allow easy backtracking.)

It's time to think about writing a recursive function.

```Haskell
-- NOT final version

type IsCompleteSolution = Bool

scanAlgoXSimple' :: SparseMatrix -> [Key] -> [([Key], SparseMatrix, IsCompleteSolution)]
...
```

`scanAlgoXSimple'` is a function taking a matrix and a partial solution (list of `Key`s) and returning a list of intermediate results: a list of state tuples of (a solution, a matrix, a boolean indicating if the solution is complete).

```Haskell
-- NOT final version

type IsCompleteSolution = Bool

scanAlgoXSimple' :: SparseMatrix -> [Key] -> [([Key], SparseMatrix, IsCompleteSolution)]
scanAlgoXSimple' m@(SparseMatrix rows activeCols) solution
    | size activeCols == 0 = [(solution, m, True)]
    | otherwise            = (solution, m, False) : []
```

- If there are no more active columns, `scanAlgoXSimple'` returns the solution, the matrix, and `True` indicating that this a complete solution.
- Otherwise, for the moment we will just return the current partial solution, the current matrix and `False` indicating that this is a partial solution. And then we don't do anything further.

```Haskell
ghci> printScan $ scanAlgoXSimple' m1 []
([],
+---++---+---+---+---+---+---+---+
|   || 1 | 2 | 3 | 4 | 5 | 6 | 7 |
+===++===+===+===+===+===+===+===+
| 0 || 1 | 0 | 0 | 1 | 0 | 0 | 1 |
| 1 || 1 | 0 | 0 | 1 | 0 | 0 | 0 |
| 2 || 0 | 0 | 0 | 1 | 1 | 0 | 1 |
| 3 || 0 | 0 | 1 | 0 | 1 | 1 | 0 |
| 4 || 0 | 1 | 1 | 0 | 0 | 1 | 1 |
| 5 || 0 | 1 | 0 | 0 | 0 | 0 | 1 |
+---++---+---+---+---+---+---+---+
,False)
```

The solution is empty, the matrix is full, and the solution is partial. And we don't go further.

```Haskell
-- NOT final version

type IsCompleteSolution = Bool

scanAlgoXSimple' :: SparseMatrix -> [Key] -> [([Key], SparseMatrix, IsCompleteSolution)]
scanAlgoXSimple' m@(SparseMatrix rows activeCols) solution
    | size activeCols == 0 = [(solution, m, True)]
    | otherwise =
        let (r, row)  = IntMap.findMin rows
            m'        = SparseMatrix (IntMap.filter (row `disjoint`) rows)
                                     (activeCols `difference` row)
        in  (solution, m, False) : scanAlgoXSimple' m' (r:solution)
```

Here we try taking the first available row. Then we reduce the matrix as we did before: the rows are filtered for being `disjoint` with the selected row and the row's 1s are removed from the active columns with `difference` - giving us the reduced matrix `m'`. We return the current state tuple and recursively return the rest of the states by calling `scanAlgoXSimple' m' (r:solution)`. We pass the reduced matrix to `scanAlgoXSimple'`, as well as prepending the selected row key `r` to the current partial solution. (Our solution list will grow backwards.)

```Haskell
ghci> printScan $ scanAlgoXSimple' m1 []
([],
+---++---+---+---+---+---+---+---+
|   || 1 | 2 | 3 | 4 | 5 | 6 | 7 |
+===++===+===+===+===+===+===+===+
| 0 || 1 | 0 | 0 | 1 | 0 | 0 | 1 |
| 1 || 1 | 0 | 0 | 1 | 0 | 0 | 0 |
| 2 || 0 | 0 | 0 | 1 | 1 | 0 | 1 |
| 3 || 0 | 0 | 1 | 0 | 1 | 1 | 0 |
| 4 || 0 | 1 | 1 | 0 | 0 | 1 | 1 |
| 5 || 0 | 1 | 0 | 0 | 0 | 0 | 1 |
+---++---+---+---+---+---+---+---+
,False)

([0],
+---++---+---+---+---+
|   || 2 | 3 | 5 | 6 |
+===++===+===+===+===+
| 3 || 0 | 1 | 1 | 1 |
+---++---+---+---+---+
,False)

([3,0],
+--++---+
|  || 2 |
+==++===+
+--++---+
,False)*** Exception: findMin: empty map has no minimal element
```

We picked row 0. Next we only had one choice, to pick row 3. Next we were left with no rows but column 2 had not been satisfied. We have not handled this case, so we got an error.

```Haskell
-- Final version

type IsCompleteSolution = Bool

scanAlgoXSimple' :: SparseMatrix -> [Key] -> [([Key], SparseMatrix, IsCompleteSolution)]
scanAlgoXSimple' m@(SparseMatrix rows activeCols) solution
    | size activeCols == 0 = [(solution, m, True)]
    | otherwise =
            (solution, m, False) : [ s | (r, row) <- IntMap.toList rows,
                                         let m' = SparseMatrix (IntMap.filter (row `disjoint`) rows)
                                                               (activeCols `difference` row),
                                         s <- scanAlgoXSimple' m' (r:solution) ]
```

Instead of picking a row out of the matrix as before, we now iterate over all rows (`(r, row) <- IntMap.toList rows`). For each row we reduce the matrix and recursively return all results for that selection, concatenating them (`s <- scanAlgoXSimple' m' (r:solution)`).

```Haskell
ghci> printScan $ scanAlgoXSimple' m1 []
([],
+---++---+---+---+---+---+---+---+
|   || 1 | 2 | 3 | 4 | 5 | 6 | 7 |
+===++===+===+===+===+===+===+===+
| 0 || 1 | 0 | 0 | 1 | 0 | 0 | 1 |
| 1 || 1 | 0 | 0 | 1 | 0 | 0 | 0 |
| 2 || 0 | 0 | 0 | 1 | 1 | 0 | 1 |
| 3 || 0 | 0 | 1 | 0 | 1 | 1 | 0 |
| 4 || 0 | 1 | 1 | 0 | 0 | 1 | 1 |
| 5 || 0 | 1 | 0 | 0 | 0 | 0 | 1 |
+---++---+---+---+---+---+---+---+
,False)

([0],
+---++---+---+---+---+
|   || 2 | 3 | 5 | 6 |
+===++===+===+===+===+
| 3 || 0 | 1 | 1 | 1 |
+---++---+---+---+---+
,False)

([3,0],
+--++---+
|  || 2 |
+==++===+
+--++---+
,False)

([1],
+---++---+---+---+---+---+
|   || 2 | 3 | 5 | 6 | 7 |
+===++===+===+===+===+===+
| 3 || 0 | 1 | 1 | 1 | 0 |
| 4 || 1 | 1 | 0 | 1 | 1 |
| 5 || 1 | 0 | 0 | 0 | 1 |
+---++---+---+---+---+---+
,False)

([3,1],
+---++---+---+
|   || 2 | 7 |
+===++===+===+
| 5 || 1 | 1 |
+---++---+---+
,False)

([5,3,1],
+--++--+
|  ||  |
+==++==+
+--++--+
,True)

([4,1],
+--++---+
|  || 5 |
+==++===+
+--++---+
,False)

([5,1],
+---++---+---+---+
|   || 3 | 5 | 6 |
+===++===+===+===+
| 3 || 1 | 1 | 1 |
+---++---+---+---+
,False)

([3,5,1],
+--++--+
|  ||  |
+==++==+
+--++--+
,True)

([2],
+--++---+---+---+---+
|  || 1 | 2 | 3 | 6 |
+==++===+===+===+===+
+--++---+---+---+---+
,False)

([3],
+---++---+---+---+---+
|   || 1 | 2 | 4 | 7 |
+===++===+===+===+===+
| 0 || 1 | 0 | 1 | 1 |
| 1 || 1 | 0 | 1 | 0 |
| 5 || 0 | 1 | 0 | 1 |
+---++---+---+---+---+
,False)

([0,3],
+--++---+
|  || 2 |
+==++===+
+--++---+
,False)

([1,3],
+---++---+---+
|   || 2 | 7 |
+===++===+===+
| 5 || 1 | 1 |
+---++---+---+
,False)

([5,1,3],
+--++--+
|  ||  |
+==++==+
+--++--+
,True)

([5,3],
+---++---+---+
|   || 1 | 4 |
+===++===+===+
| 1 || 1 | 1 |
+---++---+---+
,False)

([1,5,3],
+--++--+
|  ||  |
+==++==+
+--++--+
,True)

([4],
+---++---+---+---+
|   || 1 | 4 | 5 |
+===++===+===+===+
| 1 || 1 | 1 | 0 |
+---++---+---+---+
,False)

([1,4],
+--++---+
|  || 5 |
+==++===+
+--++---+
,False)

([5],
+---++---+---+---+---+---+
|   || 1 | 3 | 4 | 5 | 6 |
+===++===+===+===+===+===+
| 1 || 1 | 0 | 1 | 0 | 0 |
| 3 || 0 | 1 | 0 | 1 | 1 |
+---++---+---+---+---+---+
,False)

([1,5],
+---++---+---+---+
|   || 3 | 5 | 6 |
+===++===+===+===+
| 3 || 1 | 1 | 1 |
+---++---+---+---+
,False)

([3,1,5],
+--++--+
|  ||  |
+==++==+
+--++--+
,True)

([3,5],
+---++---+---+
|   || 1 | 4 |
+===++===+===+
| 1 || 1 | 1 |
+---++---+---+
,False)

([1,3,5],
+--++--+
|  ||  |
+==++==+
+--++--+
,True)
```

We have found all of the solutions. They are all permutations of the same solution {1, 3, 5}.

The output is a little too long, so let's write a function to stop after the first solution is found.

```Haskell
upToComplete :: [([Key], SparseMatrix, IsCompleteSolution)] -> [([Key], SparseMatrix, IsCompleteSolution)]
upToComplete states =
    let (partial, (complete:rest)) = break (\(_, _, isComplete) -> isComplete) states
    in  partial ++ [complete]
```

Break the list of states, cutting before the first complete solution. Return the partial solution states that came before the cut, appending the complete solution state (the first after the cut). Ignore the rest.

```Haskell
ghci> printScan $ upToComplete $ scanAlgoXSimple' m1 []
([],
+---++---+---+---+---+---+---+---+
|   || 1 | 2 | 3 | 4 | 5 | 6 | 7 |
+===++===+===+===+===+===+===+===+
| 0 || 1 | 0 | 0 | 1 | 0 | 0 | 1 |
| 1 || 1 | 0 | 0 | 1 | 0 | 0 | 0 |
| 2 || 0 | 0 | 0 | 1 | 1 | 0 | 1 |
| 3 || 0 | 0 | 1 | 0 | 1 | 1 | 0 |
| 4 || 0 | 1 | 1 | 0 | 0 | 1 | 1 |
| 5 || 0 | 1 | 0 | 0 | 0 | 0 | 1 |
+---++---+---+---+---+---+---+---+
,False)

([0],
+---++---+---+---+---+
|   || 2 | 3 | 5 | 6 |
+===++===+===+===+===+
| 3 || 0 | 1 | 1 | 1 |
+---++---+---+---+---+
,False)

([3,0],
+--++---+
|  || 2 |
+==++===+
+--++---+
,False)

([1],
+---++---+---+---+---+---+
|   || 2 | 3 | 5 | 6 | 7 |
+===++===+===+===+===+===+
| 3 || 0 | 1 | 1 | 1 | 0 |
| 4 || 1 | 1 | 0 | 1 | 1 |
| 5 || 1 | 0 | 0 | 0 | 1 |
+---++---+---+---+---+---+
,False)

([3,1],
+---++---+---+
|   || 2 | 7 |
+===++===+===+
| 5 || 1 | 1 |
+---++---+---+
,False)

([5,3,1],
+--++--+
|  ||  |
+==++==+
+--++--+
,True)
```

This is more manageable. Recall the solutions are built backwards. So we tried row 0, then we tried row 3. That did not work out, so we backtracked and tried row 1, then row 3, then row 5, and that worked out.

Everything here is lazily evaluated, so scanAlgoXSimple' stopped searching when we stopped asking for further states after receiving the complete solution.

Here is a version of the function that only returns the solutions instead of all intermediate states.

```Haskell
algoXSimple' :: SparseMatrix -> [Key] -> [[Key]]
algoXSimple' (SparseMatrix rows activeCols) solution
    | size activeCols == 0 = [solution]
    | otherwise            = [ s | (r, row) <- IntMap.toList rows,
                                   let m' = SparseMatrix (IntMap.filter (row `disjoint`) rows)
                                                         (activeCols `difference` row),
                                   s <- algoXSimple' m' (r:solution) ]
```

```Haskell
ghci> algoXSimple' m1 []
[[5,3,1],[3,5,1],[5,1,3],[1,5,3],[3,1,5],[1,3,5]]

ghci> head $ algoXSimple' m1 []
[5,3,1]
```

Let's appreciate for a moment, how far we have come so quickly, and with such beautiful terse code.

Compare this for compactness and simplicity with Dancing Links, which is how Algorithm X is usually implemented.

https://www.wikiwand.com/en/Dancing_Links

## Column selection

Now, as Wikipedia says: to reduce the number of iterations, Knuth suggests that the column-choosing algorithm select a column with the smallest number of 1s in it.

I'm going to give this one all in one go:

```Haskell
(+.) = IntMap.unionWith (+)

algoX m = algoX' m []

algoX' :: SparseMatrix -> [Key] -> [[Key]]
algoX' (SparseMatrix rows activeCols) solution
    | size activeCols == 0 = [solution]
    | otherwise =
        let (c, colSum) = selectedColumn
        in  [ s | colSum > 0,
                  (r, row) <- IntMap.toList rows,
                  let m' = SparseMatrix (IntMap.filter (row `disjoint`) rows)
                                        (activeCols `difference` row),
                  s <- algoX' m' (r:solution) ]
        where
            withOnes row   = IntMap.fromSet (\_ -> 1) $ row `intersection` activeCols
            colSums        = IntMap.foldl (\sums row -> sums +. withOnes row)
                                          (IntMap.fromSet (\_ -> 0) activeCols)
                                          rows
            selectedColumn = snd . minimum $ map (\(key, cSum) -> (cSum, (key, cSum))) $ IntMap.toList colSums
```

Playing in the interpreter with some of the new expressions is left as an exercise to the reader.

This version is little verbose with selecting the column, but the main part of the algorithm remains clear. (Also, we could be caching some things to make the column selection computation more efficient...)

```Haskell
ghci> head $ algoX m1
[5,3,1]
```

Turns out the solution is the same in our toy case as with `algoXSimple'`.

If there is interest, we can throw these algorithms at some large Sudoku-style puzzles in some future post.

Holy-moly, did you really make it to the end? You are a true hero.


## Appendix

### Printing functions

```Haskell
(!.) :: Num a => Row -> Key -> a
row !. key
    | member key row = 1
    | otherwise      = 0

toFilledList :: Num a => ActiveCols -> Row -> [a]
toFilledList activeCols row = map (row !.) $ elems activeCols

instance Show SparseMatrix where
    show (SparseMatrix rows activeCols) =
        ('\n' :) $ render show show show $ Table (Group NoLine     $ map Header $ IntMap.keys rows)
                                                 (Group SingleLine $ map Header $ elems activeCols)
                                                 (map (toFilledList activeCols) $ IntMap.elems rows)

printRow :: Row -> IO ()
printRow row = print $ SparseMatrix (IntMap.fromList [(0, row)]) row

oneColumnTable :: String -> [(key, value)] -> Table key String value
oneColumnTable columnHeader ls = Table (Group NoLine $ map (Header . fst) ls)
                                       (Header columnHeader)
                                       (map (\(_, s) -> [s]) ls)

printList :: Show a => String -> [a] -> IO ()
printList title = putStrLn . render show id show . oneColumnTable title . zip [0..]

printScan :: Show a => [a] -> IO ()
printScan = putStrLn . intercalate "\n\n" . map show
```
