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
