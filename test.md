# A Stateless Paradigm

## Functional purity and the absence of states

One of the most distinguishing aspects of functional programming and the declarative family of languages is its philosophy of statelessness.
A programmer primarily exposed to mutable imperative programming languages will find that the concept of state is natural and maybe even inevitable.
Functional paradigm challenges this concept and offers a much safer and mathematically intuitive philosophy.
In the perspective of functional programming, there is no state, and everything is immutable.

### Pure functions

To understand the functional programming perspective, we have to take a step away from the imperative programming definition of a function.
Let's go back to the definition of a function in mathematics.

Across several, mathematical disciplines a function means the same thing.
Consider two sets $A$ and $B$.
We can define a function as the *mapping* between the elements of $A$ and $B$.
The elements of $A$ and $B$ can be anything, they can be numbers, which shows us how a function can be represented by a formula or a graph.
The elements of $A$ and $B$ can be matrices and vectors, which defines a function as a transformation between two vector spaces.
On the higher level perspective of category theory, functions are *morphisms* between objects of a given category.

$$
f:(A\to B)
$$

If you remember functions from discrete math, functions at its most basic form looks like the one above.

Functions in functional programming languages like Haskell are (arguably) the closest computer representation of a mathematical function.
We call these functions, **pure functions**.

They differ from your standard C function because the definition inside pure functions are only instructions on how to produce a result based on the parameters.
To fully understand this concept here are some examples of impure functions

```c
int square(int x){
	addToExternalLogger("calculating square");
    return x*x;
}
```

The impurity in this function is the line where the function writes to some external logger, `addToExternalLogger("calculating square");`.
A function can only be pure if the result of the function can be fully determined by its parameter.
The only parameter here is `x`.
Invoking this square function with the same parameter value does not do the same thing.
The effect of changing the logger is dependent on the previous state of the external logger.
The effect on the logger is what we call a **side effect** of the `square` function.
It is a side effect since this line of code modifies values outside boundaries of the function.

```c
int* increaseArray(int *a, int size){
    for(int i = 0; i < size; i++)
        a[i] = a[i]+1;
}
```

A pass by address/reference function which changes the value of a parameter will automatically be an impure function since changing the value of `a` is a **mutation**, which is a side effect.


```kotlin
fun headsortails(n: Int) {
    val results: MutableList<String> = mutableListOf()
	if ((0..1).random() == 0)
		results.add("Heads")
	else
        results.add("Tails")
    return results
}
```

This function is also impure because the return value is not dependent on the parameters alone.
The return value will be dependent on the randomization seed which is something outside the parameters of the function.

A pure function must satisfy these two:

1. A pure function has no side effects
2. A pure functions output must be dependent on the inputs alone[^function]

[^function]: In fact if $f(a)=b$ and $f(a)=c$ where $b\neq c$, then $f$ is not a function at all

A good way to test if a function is pure is if you can (theoretically) create an infinitely long *lookup table* such that, looking up the value for a specific input is perfectly identical to calling the function with the same input.
And if you think about it this is the essence of a function.
Functions are just of mappings between the domain and the range.

For example, a `square :: Int -> Int` function can be replaced by such look-up table:

| Domain (`x :: Int`) | Range (`(square x) :: Int`) |
|:---:|:---:|
| $\vdots$ | $\vdots$ |
| $-2$     |  $4$     |
| $-1$     |  $1$     |
|  $0$     |  $0$     |
|  $1$     |  $1$     |
|  $2$     |  $4$     |
| $\vdots$ | $\vdots$ |

Compare this with the `headsortails()` function in kotlin, which cannot be represented by a lookup table, since the same input can result to different output.

| Domain (`n :: Int`) | Range (`(headsortails(n)) :: MutableList<String>`) |
|:---:|:---:|
| $0$      |  `[]`     |
| $1$      |  `["Heads"]`     |
| $1$      |  `["Tails"]`     |
| $2$      |  `["Heads", "Heads"]`     |
| $2$      |  `["Heads", "Tails]"`     |
| $\vdots$ | $\vdots$ |

### Bindings vs Assignment and Referential Transparency

One of the defining features of imperative programming is the assignment statement.
It enables the program to advance to a new state.
Purely functional programming languages like Haskell *do not have assignment statements*.
Therefore, it lacks the mechanism to mutate anything.
Using the "`=`" operator (which signals an assignment statement in imperative languages) in functional languages *binds* the value on the right-hand side to the left-hand side.
This mechanism is conceptually different from an assignment operation in C.
It is perfectly fine to do the following in C:

```C
int x = 0;
x = 1;
``` 
