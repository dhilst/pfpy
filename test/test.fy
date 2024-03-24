# hello comments!

# constants
1;
"hello world";
true;

# variables
x;
my_variable;

# function calls
f();
f(x);
f(x,1);
f(g(x), true, "hello", 1);

# function definitions, with optional generic arguments
def id_int(x: int): int = x;
def id_int[T](x: T): T = x;
def id_int[T: int](x: T): T = x;

# imports
import math;
from op import mul, sub, eq;

def dsrqt(x: float): float =
    op.mul(math.sqrt(x), 2);

# types
type Opt[T] = Union[Some[T], None];
type Resut[A, B] = Union[Ok[A], Err[B]];

def ret_some(x: int): Opt[int] = Some(x);
def bind_some(x: Opt[int], f: Arrow[int, Opt[int]]): Opt[int] =
    match x with
    | Some(x) => f(x)
    | None => None     
    end;

def fact(x: int): int =
    if eq(x, 1) then 1 else mul(x, fact(sub(x, 1)));

def opt_add[A](a: Opt[A], b: Opt[A]): Opt[A] =
    match a with
    | None => None
    | Some(a) =>
        match b with
        | Some(b) => Some(add(a, b))
        | None => Some(a)
        end
    end;

# @TODO dicts, lists, tuples and sets notation
# @TODO lambda notation
# @TODO let notation
# @TODO infix operators
# @TODO let monadic notation

