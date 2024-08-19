# https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State

data TurnstileState = Union[Locked, Unlocked];
data TurnstileOutput = Union[Thank, Open, Tut];

def coin(x: TurnstileState): tuple[TurnstileOutput, TurnstileState] =
  (Thank(), Unlocked());

def push(x: TurnstileState): tuple[TurnstileOutput, TurnstileState] =
  match x with
  | Locked() => (Tut(), Locked())
  | Unlocked() => (Open(), Locked())
  end;

def monday(
  s0: TurnstileState
): tuple[list[TurnstileOutput], TurnstileState] =
  let (a1, s1) = coin(s0) in
  let (a2, s2) = push(s1) in
  let (a3, s3) = push(s2) in
  let (a4, s4) = coin(s3) in
  let (a5, s5) = push(s4) in
  ([a1, a2, a3, a4, a5], s5);
  

  print(monday(Locked()));
