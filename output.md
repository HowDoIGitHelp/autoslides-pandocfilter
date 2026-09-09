# A Stateless Paradigm

---

# Functional purity and the absence of states

---

# Pure functions

- We can define a function as the *mapping* between the elements of $A$
  and $B$.
- On the higher level perspective of category theory, functions are
  *morphisms* between objects of a given category.

---

# Pure functions

`$$
f:(A\to B)
$$`

---

# Pure functions

- We call these functions, **pure functions**.

---

# Pure functions

``` c
int square(int x){
    addToExternalLogger("calculating square");
    return x*x;
}
```

---

# Pure functions

- The effect on the logger is what we call a **side effect** of the
  `square` function.

---

# Pure functions

``` c
int* increaseArray(int *a, int size){
    for(int i = 0; i < size; i++)
        a[i] = a[i]+1;
}
```

---

# Pure functions

- A pass by address/reference function which changes the value of a
  parameter will automatically be an impure function since changing the
  value of `a` is a **mutation**, which is a side effect.

---

# Pure functions

``` kotlin
fun headsortails(n: Int) {
    val results: MutableList<String> = mutableListOf()
    if ((0..1).random() == 0)
        results.add("Heads")
    else
        results.add("Tails")
    return results
}
```

---

# Pure functions

1.  A pure function has no side effects
2.  A pure functions output must be dependent on the inputs alone

---

# Pure functions

- A good way to test if a function is pure is if you can (theoretically)
  create an infinitely long *lookup table* such that, looking up the
  value for a specific input is perfectly identical to calling the
  function with the same input.

---

# Pure functions

| Domain (`x :: Int`) | Range (`(square x) :: Int`) |
|:-------------------:|:---------------------------:|
|      $\vdots$       |          $\vdots$           |
|        $-2$         |             $4$             |
|        $-1$         |             $1$             |
|         $0$         |             $0$             |
|         $1$         |             $1$             |
|         $2$         |             $4$             |

---

# Pure functions

| Domain (`x :: Int`) | Range (`(square x) :: Int`) |
|:-------------------:|:---------------------------:|
|      $\vdots$       |          $\vdots$           |

---

# Pure functions

| Domain (`n :: Int`) | Range (`(headsortails(n)) :: MutableList<String>`) |
|:-------------------:|:--------------------------------------------------:|
|         $0$         |                        `[]`                        |
|         $1$         |                    `["Heads"]`                     |
|         $1$         |                    `["Tails"]`                     |
|         $2$         |                `["Heads", "Heads"]`                |
|         $2$         |                `["Heads", "Tails]"`                |
|      $\vdots$       |                      $\vdots$                      |

---

# Bindings vs Assignment and Referential Transparency

- Purely functional programming languages like Haskell *do not have
  assignment statements*.
- Using the "`=`" operator (which signals an assignment statement in
  imperative languages) in functional languages *binds* the value on the
  right-hand side to the left-hand side.

---

# Bindings vs Assignment and Referential Transparency

``` c
int x = 0;
x = 1;
```

---

# Testnestedlist

- weqwe

- 12312

- 3123123

- 231231

- asdasd

---

# Testnestedlist

- sdada

  - 213123
  - 2312313

---
