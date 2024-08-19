from typing import Callable, Union, cast
from dataclasses import dataclass

@dataclass
class Ok[T]:
    val0: T

@dataclass
class Error[E]:
    val0: E

type Result[T, E] = Union[Ok[T], Error[E]]

def result_bind[A, B, E](x: Result[A, E], f: Callable[[A], Result[B, E]] ) -> Result[B, E]:
    match x:
        case Ok(y):
            return f(y)
        case Error(e):
            return Error(e)

assert(result_bind(Ok(1), (lambda x: result_bind(Ok(2), (lambda y: Ok(x + y)))))) == Ok(3)

def result_maperr[A, E1, E2](x: Result[A, E1], f: Callable[[E1], E2] ) -> Result[A, E2]:
    match x:
        case Ok(y):
            return Ok(y)
        case Error(e):
            return Error(f(e))

def result_binderr[A, B, E1, E2](x: Result[A, E1], f: Callable[[A], Result[B, E2]] , ferr: Callable[[E1], E2] ) -> Result[B, E2]:
    return result_bind(result_maperr(x, ferr), f)

@dataclass
class Some[T]:
    val0: T

@dataclass
class Nothing:
  pass

type Opt[T] = Union[Some[T], Nothing]

def opt_bind[A, B](x: Opt[A], f: Callable[[A], Opt[B]] ) -> Opt[B]:
    match x:
        case Some(y):
            return f(y)
        case Nothing():
            return Nothing()

assert(opt_bind(Some(1), (lambda x: opt_bind(cast(Opt[int], Nothing()), (lambda y: Some(x + y))))) == Nothing())

@dataclass
class Pair[A, B]:
    val0: A
    val1: B

type PairT[A, B] = Pair[A, B]

def pair_swap[A, B](p: PairT[A, B]) -> PairT[B, A]:
    match p:
        case Pair(x, y):
            return Pair(y, x)

assert(pair_swap(Pair(1, "true")) == Pair("true", 1))

@dataclass
class PairInt[int, int]:
    val0: int
    val1: int

type PairIntT = PairInt[int, int]
