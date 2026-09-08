### Pure functions

- We can define a function as the *mapping* between the elements of $A$
  and $B$.
- On the higher level perspective of category theory, functions are
  *morphisms* between objects of a given category.

---

### Pure functions

`$$
f:(A\to B)
$$`

---

### Pure functions

- We call these functions, **pure functions**.

---

### Pure functions

``` c
int square(int x){
    addToExternalLogger("calculating square");
    return x*x;
}
```

---

### Pure functions

- The effect on the logger is what we call a **side effect** of the
  `square` function.

---

### Pure functions

``` c
int* increaseArray(int *a, int size){
    for(int i = 0; i < size; i++)
        a[i] = a[i]+1;
}
```

---

### Pure functions

- A pass by address/reference function which changes the value of a
  parameter will automatically be an impure function since changing the
  value of `a` is a **mutation**, which is a side effect.

---

### Pure functions

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

### Pure functions

1.  A pure function has no side effects
2.  A pure functions output must be dependent on the inputs alone[^1]

---

### Pure functions

- A good way to test if a function is pure is if you can (theoretically)
  create an infinitely long *lookup table* such that, looking up the
  value for a specific input is perfectly identical to calling the
  function with the same input.

---

### Pure functions

   Domain (`x :: Int`)   Range (`(square x) :: Int`)
  --------------------- -----------------------------
        $\vdots$                  $\vdots$
          $-2$                       $4$
          $-1$                       $1$
           $0$                       $0$
           $1$                       $1$
           $2$                       $4$
        $\vdots$                  $\vdots$

---

### Pure functions

  ----------------------------------------------------------------------------------
          Domain (`n :: Int`)                             Range
                                       (`(headsortails(n)) :: MutableList<String>`)
  ----------------------------------- ----------------------------------------------
                  $0$                                      `[]`

                  $1$                                  `["Heads"]`

                  $1$                                  `["Tails"]`

                  $2$                              `["Heads", "Heads"]`

                  $2$                              `["Heads", "Tails]"`

               $\vdots$                                  $\vdots$
  ----------------------------------------------------------------------------------

---

### Bindings vs Assignment and Referential Transparency

- Purely functional programming languages like Haskell *do not have
  assignment statements*.
- Using the "`=`" operator (which signals an assignment statement in
  imperative languages) in functional languages *binds* the value on the
  right-hand side to the left-hand side.

---

### Bindings vs Assignment and Referential Transparency

``` c
int x = 0;
x = 1;
```

---

### Bindings vs Assignment and Referential Transparency

- The second line, **mutates** the value stored in address of `x` to the
  new value `1`.
- Variables in imperative programming **depend on the current state** of
  the program.

---

### Bindings vs Assignment and Referential Transparency

![States](../mermaid_diagrams/state_changes.png)

---

### Bindings vs Assignment and Referential Transparency

``` haskell
x = 0
x = 1
```

---

### Bindings vs Assignment and Referential Transparency

``` haskell
main.hs:2:1: error:
  Multiple declarations of ‘x’
  Declared at: main.hs:1:1
         main.hs:2:1
```

---

### Bindings vs Assignment and Referential Transparency

- In Haskell, any "`=`" statement is a declaration of a **binding**.
- For example, with `x = 0`, `x` is now **bound** to the value `0`.
- These bindings are **final** within its scope.
- Because of this you can predict the evaluation of any expression
  simply by *replacing* the variable with its bound value.
- This property is known as **referential transparency**
  [@hughes_why_1989].

---

## Consequences of Functional Purity, Statelessness, and Immutability

- Eliminating all side effects is demonstrably *safer* against
  accidental errors.

---

## Consequences of Functional Purity, Statelessness, and Immutability

``` c
void f(int *x, int y){
    *x = *x + y;
    printf("%d\n",*x);
    return;
}
int main(void) {
    int x = 0;
    f(&x, 3);
    x = x - 2;
    f(&x, 3);
}
```

---

## Consequences of Functional Purity, Statelessness, and Immutability

- In the previous example, the exact behavior of the function `f()`
  **depends on where you use it**.

---

## Consequences of Functional Purity, Statelessness, and Immutability

- On a corporate setting where multiple people are working on the same
  codebase, refactoring becomes *unsafe* without knowledge of all the
  side effects of the functions in use.
- On systems with *shared resources* and multi-threading it becomes even
  more difficult to keep track of things without proper documentation.
- These are the consequences of **referential opacity**, the opposite of
  referential transparency.

---

## Consequences of Functional Purity, Statelessness, and Immutability

- It sacrifices assignment statements and all its derived capabilities
  to prioritize *safety and readability*.

---

### Statelessness as a paradigm

- Applying the paradigm of statelessness is just to writing code with
  more *discipline*.

---

[^1]: In fact if $f(a)=b$ and $f(a)=c$ where $b\neq c$, then $f$ is not
    a function at all
