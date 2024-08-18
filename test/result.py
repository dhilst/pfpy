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
