# Kojun

```
┌───────┬───────┬───┬───┬───────┐
│ -   - │ -   - │ - │ - │ -   - │
│       ├───┐   ├───┤   └───┐   │
│ -   1 │ 3 │ - │ - │ -   - │ - │
├───────┘   ├───┤   ├───┬───┴───┤
│ -   -   - │ - │ - │ 3 │ -   - │
├───────────┤   │   │   └───┐   │
│ -   -   3 │ - │ - │ -   - │ - │
├───┬───────┘   └───┤       │   │
│ - │ 5   -   3   - │ -   - │ - │
│   ├───┬───────────┼───┬───┤   │
│ - │ 2 │ -   -   - │ - │ - │ - │
├───┘   └───────┬───┤   └───┴───┤
│ -   -   -   - │ - │ -   3   - │
├───┬───────────┘   │   ┌───────┤
│ - │ -   5   3   - │ - │ -   - │
└───┴───────────────┴───┴───────┘
```

Kojun is a logic puzzle with appearance similar to Sudoku. There are three rules:

- Each region of size N must contain each number from 1 to N exactly once.
- Numbers in orthogonally adjacent cells must be different.
- If two cells are vertically adjacent in the same region, the number in the upper cell must be greater than the number in the lower cell.

Replicating this puzzle's solver in different languages is a good way to get used to the language; although it's a simple algorithm, it deals with many fundamental concepts. This means it's specially good to learn more exoteric languages, since the logic won't get in the way.

The solution to the puzzle displayed is as follows:

```
4 2 1 3 1 2 1 3
3 1 3 2 3 1 3 2 
2 4 1 6 2 3 1 5 
1 2 3 4 1 2 5 4 
2 5 1 3 2 1 4 3 
1 2 3 2 1 5 1 2 
3 1 4 5 2 4 3 1 
1 4 5 3 1 2 1 2 
```