# hello comments!
# statements are terminated by ;

# constants
1;
2;
"hello world";
true;

# variables
x;
my_variable;
math.pi;
 
# function calls
f();
a[0];
f(x);
f(x,1);
f(g(x), true, "hello", 1);

# tuples
(1,2,3);
(1,);

# lists
[1,2,3];

# set
{1, 2, 3};

# dicts
{ "foo": "bar", "tar": "zar" };
# 
# # lamdbas
lambda (x: int, y: int) => f(y, x);

# infix operators
1 + 2;
1 + 3 + 3;
1 + 2 * 3 - 4;

# parenthesized expressions
2 * (3 + 4);

# binary operations
1 & 2;
 
# boolean operators
1 == 1;
1 != 1;
1 >  1;
1 >= 1;
1 <  1;
1 <= 1;

# let expressions
let x = 1 in x + 1;

# function definitions, with optional generic arguments
def id_int(x: int): int = x;
def id_int[T](x: T): T = x;
def id_int[T: int](x: T): T = x;
def id_byte_or_string[T: (str, bytes)](x: T): T = x;
def id_byte_or_string(x: str ^ bytes): T = x;
# 
# # imports
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
# 
type Opt[T] = Some[T] ^ None;
def bind_some2(x: Opt[int], f: int -> Opt[int]): Opt[int] =
    match x with
    | Some(x) => f(x)
    | None => None     
    end;

# monadic let assigning
let (let*) = bind_some2;
# 
# # monadic let usage
let* x = Some(1) in
let* y = Some(2) in
Some(x + y);
# 
let (let*) = bind_some2 in
let* w = Some(1) in
let* z = Some(2) in
Some(w + w);

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

def opt_add2(a: Opt[int], b: Opt[int]): Opt[int] =
    let (let*) = bind_some2 in
    let* x = a in
    let* y = b in
    Some(x + y);

# Reduces to this
def opt_add2(a: Opt[int], b: Opt[int]): Opt[int] =
    bind_some2(a, lambda (x: int) =>
        bind_some2(b, lambda (y: int) =>
            Some(x + y)));

