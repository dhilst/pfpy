# Pure Functional Python

PFPy aims to transpile to Python (3.12), while providing 100% type coverage.
The language syntax try to be familiar to Python programmers while borrowing
some syntax from OCaml. The transpiled code is fully typed.

The goal is to enable formally verifying Python code by placing restrictions
on the code itself.

The available Python constructs are:

* Functions 
* Match statements
* If statements
* Dataclasses
* Python expressions

The unavailable Python costructs (not an exaustive list) are:

* No classes
* No exceptions
* No methods
* No iterators

Also all functions are:
* Fully typed, supporting type parameters
* Consists of a single expression

Let's se an example:

```
# test/result.fy
from typing import Callable, Union;
from dataclasses import dataclass;

data Result[T, E] = Union[Ok[T], Error[E]];

def bind_result[A, B, E](
  x: Result[A, E],
	f: A -> Result[B, E]
): Result[B, E] =
  match x with
  | Ok(y) => f(y)
  | Error(e) => Error(e)
  end;

print(
  bind_result(Ok(1), lambda (x: int) =>
  bind_result(Ok(2), lambda (y: int) =>
  Ok (x + y))));
``` 

This code is translated to this Python code:

```python
from typing import Callable, Union

from dataclasses import dataclass

@dataclass
class Ok[T]:
    value0: T

@dataclass
class Error[E]:
    value0: E

type Result[T, E] = Union[Ok[T], Error[E]]

def bind_result[A, B, E](x: Result[A, E], f: Callable[[A], Result[B, E]]) -> Result[B, E]:
    match x:
        case Ok(y):
            return f(y)
        case Error(e):
            return Error(e)

print(bind_result(Ok(1), (lambda x: bind_result(Ok(2), (lambda y: Ok(x + y))))))
```

The most "magical" construct of PFPy is the `data` statement, which expands
type classes for each type constructor. Also the arrow operator expands to
`Callable` calls making function types more readable.

To translate PFPy code use the stdin, e.g.:

```
pfpy < test/test.fy
```

To build and install you'll need dune and opam

```
dune install
```

After that `pfpy` will be in your path
