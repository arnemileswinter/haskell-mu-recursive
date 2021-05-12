# Haskell Mu Recursion

This project aims to clear headache of understanding how mu-recursive calculation works.
Mu-Recursive programs can be written by composition of the pre-defined operators. 

Namely, these are:

### Primitives

##### Null

The null function takes a number of arguments and always yields 0.

```hs
null :: Int -> Mu
```

##### Succ

The successor function takes exactly one argument and always yields the successor.

```hs
succ :: Mu
```

formally:
[succ](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D%20%5Cbegin%7Balign*%7Dsucc:%20%5Cmathbb%7BN%7D%20&%5Cto%20%5Cmathbb%7BN%7D%20%5C%5C%20%20%20%20%20%20n%20&%5Cmapsto%20n&plus;1%5Cend%7Balign*%7D)

##### Projection

The Projection Operator is constructed by supplying a positive number indicating how many parameters it will receive, and a positive number indicating which parameter to project onto.

```hs
proj :: Int -> Int -> Mu
```

Intuitively it is a selection operator, where the first argument (k) specifies what size of tuple it receives and the second argument (i) specifying which element of the tuple to select. 

formally:

[equation](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D%20%5Cbegin%7Balign*%7Dproj_%7Bk%20%5Crightarrow%20i%7D:%20%5Cmathbb%7BN%7D%5Ek%20&%5Cto%20%5Cmathbb%7BN%7D%20%5C%5C%20(n_1,n_2,...,n_i,...,n_k)%20&%5Cmapsto%20%20n_i%5Cend%7Balign*%7D)


##### Composition

Mu-Recursive Programs can be composed to yield more complex structures.
In Haskell this can be used:

```hs
(<@>) :: Mu -> [Mu] -> Mu
```

With the aforementioned `null` and `succ` functions, we can now declare a constant `one` function.
```hs
one :: Mu
one = succ <@> [null 0]
```

In this example, the `succ` is composed with the `null 0` function, such that the resulting function retrieves 0 parameters.

##### Primitive Recursion

Primitive recursion is a concept that ressembles an always-terminating loop - like your classic for-loop in imperative languages.
The accompanying operator is:

```hs
pr :: Mu -> Mu -> Mu
```

Where the first argument can be thought of as the initialization of your iteration, or the base-case in a recursive program. The second argument is the Program that formulates the recursion step. It always receives the previous result as its first argument and the iteration-variable as its last argument.

With this operator we can now define addition and multiplication of two numbers:

```hs
add :: Mu
add = pr(proj 1 1, succ <@> [proj 3 1])

mult :: Mu
mult = pr(null 1, add <@> [proj 3 1, proj 3 2])
```

##### µ-Recursion

While most problems can be solved using primitive recursion, there are some that can only be solved with mu recursion. Think of mu-recursion as a stateful while loop that brute-forcefully finds the smallest number by incrementing a variable parameter such that a function yields 0, returning you the number of steps this took.

Note that µ-Recursion is a partial function and may not terminate, if no parameter makes the brute-forced function return 0.

To brute-force the smallest argument that makes a function return 0, you can use the Mu operator:

```hs
µ :: Mu -> Mu
```

### Running the Programs

The function `runMuIO` is defined as this:

```hs
runMuIO :: Mu -> [Int] -> IO ()
```

it receives a Mu-Recursive function and a list of arguments to supply to it - then it prints the Result along with the calculation steps to your Terminal buffer. This function is very useful for illustrating how a Mu-recursive function operates.

Alongside, there is 
```hs
runMu :: Mu -> [Int] -> Either String (String,Int)
```

It does the same as `runMuIO`, without printing to your terminal.
If it returns Left, then an error has occured running your mu-recursive function. 
If it returns Right, the first element of the tuple is the string-log of calculation steps and the second element is the numerical result of the function evaluation.
