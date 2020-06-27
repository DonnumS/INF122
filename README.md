# My Assigments from INF122 Functional Programming

## Oblig1

Performs differnt operations on strings



## Oblig2

Centered around AST (Abstract Syntax Trees)

Reads a string and creates the AST from this string. ie:
```parse + 4 * 8 - 5"```
returns the AST:
``` Sum (Tall 4) (Mult (Tall 8) (Min (Tall 5)))```

Then evaluate this AST:
```evi (parse "+ 4 * 8 - 5")```
```-36```



## Oblig3

Implementation of the game [NIM](https://en.wikipedia.org/wiki/Nim)
