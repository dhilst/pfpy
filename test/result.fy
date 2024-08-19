data Result[T, E] = Union[Ok[T], Error[E]];

def result_bind[A, B, E](
  x: Result[A, E],
	f: (A,) -> Result[B, E]
): Result[B, E] =
  match x with
  | Ok(y) => f(y)
  | Error(e) => Error(e)
  end;

assert (
  result_bind(Ok(1), lambda (x: int) =>
  result_bind(Ok(2), lambda (y: int) =>
  Ok (x + y)))) == Ok(3);

def result_maperr[A, E1, E2](
  x: Result[A, E1],
  f: (E1,) -> E2
): Result[A, E2] =
  match x with
  | Ok(y) => Ok(y)
  | Error(e) => Error(f(e))
  end;

def result_binderr[A, B, E1, E2](
  x: Result[A, E1],
  f: (A,) -> Result[B, E2],
  ferr: (E1,) -> E2
): Result[B, E2] =
  result_bind(result_maperr(x, ferr), f);

data Opt[T] = Union[Some[T], Nothing];

def opt_bind[A, B](
  x: Opt[A],
  f: (A,) -> Opt[B]
): Opt[B] =
  match x with
  | Some(y) => f(y)
  | Nothing() => Nothing()
  end;

assert(
  opt_bind(Some(1), lambda (x: int) =>
  # cast is required because Nothing()
  # does not carry enough information
  opt_bind(cast(Opt[int], Nothing()), lambda (y: int) =>
  Some (x + y))) == Nothing());

    
data PairT[A, B] = Pair[A, B];

def pair_swap[A, B](p: PairT[A, B]): PairT[B, A] =
  match p with
  | Pair(x, y) => Pair(y, x)
  end;

assert((pair_swap(Pair(1, "true"))) == Pair("true", 1));

data PairIntT[T] = Pair[int, T];

