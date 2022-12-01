open Ast
open Fmt

type cmd =
  | CONST of int
  | ADD
  | SUB
  | MUL
  | DIV
  | ACCESS of int
  | CLOSURE of cmds
  | IFTE of cmds * cmds
  | LET
  | ENDLET
  | APPLY
  | RETURN
and cmds = cmd list
[@@deriving show {with_path = false}]

type value =
  | VInt of int
  | VClo of cmds * (env[@opaque])
[@@deriving show {with_path = false}]

and stack = value list
and env = value list

let get_index k xs =
  let rec loop i xs =
    match xs with
    | [] -> None
    | x :: xs ->
      if x = k then
        Some i
      else
        loop (i + 1) xs
  in loop 0 xs

let rec compile xs m =
  match m with
  | Int i -> [ CONST i ]
  | Add (m, n) -> compile xs m @ compile xs n @ [ ADD ]
  | Sub (m, n) -> compile xs m @ compile xs n @ [ SUB ]
  | Mul (m, n) -> compile xs m @ compile xs n @ [ MUL ]
  | Div (m, n) -> compile xs m @ compile xs n @ [ DIV ]
  | Var x -> (
      match get_index x xs with
      | Some i -> [ ACCESS i ]
      | None -> failwith "unbound var(%s)" x)
  | Fun (f, x, m) ->
    let cmds = compile (f :: x :: xs) m in
    [ CLOSURE (cmds @ [ RETURN ]) ]
  | Ifte (cond, m, n) ->
    let cmds1 = compile xs m in
    let cmds2 = compile xs n in
    compile xs cond @ [ IFTE (cmds1, cmds2) ]
  | Let (x, m, n) ->
    compile xs m @ [ LET ] @ compile (x :: xs) n @ [ ENDLET ]
  | App (m, n) ->
    compile xs m @ compile xs n @ [ APPLY ]

let rec secd (s : stack) (e : env) (c : cmds) =
  match c with
  | CONST i :: c ->
    secd (VInt i :: s) e c
  | ADD :: c -> (
      match s with
      | VInt j :: VInt i :: s ->
        secd (VInt (i + j) :: s) e c
      | _ -> failwith "add")
  | SUB :: c -> (
      match s with
      | VInt j :: VInt i :: s ->
        secd (VInt (i - j) :: s) e c
      | _ -> failwith "sub")
  | MUL :: c -> (
      match s with
      | VInt j :: VInt i :: s ->
        secd (VInt (i * j) :: s) e c
      | _ -> failwith "mul")
  | DIV :: c -> (
      match s with
      | VInt j :: VInt i :: s ->
        secd (VInt (i / j) :: s) e c
      | _ -> failwith "div")
  | ACCESS i :: c ->
    secd (List.nth e i :: s) e c
  | CLOSURE c' :: c ->
    secd (VClo (c', e) :: s) e c
  | IFTE (c1, c2) :: c -> (
      match s with
      | VInt i :: s ->
        if 0 < i then
          secd s e (c1 @ c)
        else
          secd s e (c2 @ c)
      | _ -> failwith "ifte")
  | LET :: c -> (
      match s with
      | v :: s -> secd s (v :: e) c
      | _ -> failwith "let")
  | ENDLET :: c -> (
      match e with
      | _ :: e -> secd s e c
      | _ -> failwith "endlet")
  | APPLY :: c -> (
      match s with
      | v :: (VClo (c', e') as clo) :: s ->
        secd (VClo (c, e) :: s) (clo :: v :: e') c'
      | _ -> failwith "app")
  | RETURN :: c -> (
      match s with
      | v :: VClo (c', e') :: s ->
        secd (v :: s) e' c'
      | _ -> failwith "return")
  | [] -> (
      match s with
      | v :: _ -> v
      | _ -> failwith "secd")
