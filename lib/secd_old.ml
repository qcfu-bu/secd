module Env = Map.Make(Int)
open Ast

type cmd =
  | CONST of int
  | ADD
  | SUB
  | MUL
  | DIV
  | ACCESS of int
  | CLOSURE of int * int * cmds
  | IFTE of cmds * cmds
  | LET of int
  | APPLY
and cmds = cmd list
[@@deriving show {with_path = false}]

type value =
  | VInt of int
  | VClo of int * int * cmds * (value Env.t[@opaque])
[@@deriving show {with_path = false}]

let rec compile m =
  match m with
  | Int i -> [ CONST i ]
  | Add (m, n) -> compile m @ compile n @ [ ADD ]
  | Sub (m, n) -> compile m @ compile n @ [ SUB ]
  | Mul (m, n) -> compile m @ compile n @ [ MUL ]
  | Div (m, n) -> compile m @ compile n @ [ DIV ]
  | Var x -> [ ACCESS (Hashtbl.hash x) ]
  | Fun (f, x, m) ->
    let f = Hashtbl.hash f in
    let x = Hashtbl.hash x in
    let cmds = compile m in
    [ CLOSURE (f, x, cmds) ]
  | Ifte (cond, m, n) ->
    let cmds1 = compile m in
    let cmds2 = compile n in
    compile cond @ [ IFTE (cmds1, cmds2) ]
  | Let (x, m, n) ->
    let x = Hashtbl.hash x in
    compile m @ [ LET x ] @ compile n
  | App (m, n) ->
    compile m @ compile n @ [ APPLY ]

let env0 = Env.empty

let rec secd (s : value list) (e : value Env.t) (c : cmds) d =
  match c with
  | CONST i :: c ->
    secd (VInt i :: s) e c d
  | ADD :: c -> (
      match s with
      | VInt j :: VInt i :: s ->
        secd (VInt (i + j) :: s) e c d
      | _ -> failwith "add")
  | SUB :: c -> (
      match s with
      | VInt j :: VInt i :: s ->
        secd (VInt (i - j) :: s) e c d
      | _ -> failwith "sub")
  | MUL :: c -> (
      match s with
      | VInt j :: VInt i :: s ->
        secd (VInt (i * j) :: s) e c d
      | _ -> failwith "sub")
  | DIV :: c -> (
      match s with
      | VInt j :: VInt i :: s ->
        secd (VInt (i / j) :: s) e c d
      | _ -> failwith "sub")
  | ACCESS i :: c ->
    secd (Env.find i e :: s) e c d
  | CLOSURE (f, x, c') :: c ->
    secd (VClo (f, x, c', e) :: s) e c d
  | APPLY :: c -> (
      match s with
      | v :: (VClo (f, x, c', e') as clo) :: s ->
        secd [] (Env.add x v (Env.add f clo e')) c' ((s, e, c) :: d)
      | _ -> failwith "call")
  | LET x :: c -> (
      match s with
      | v :: s ->
        secd s (Env.add x v e) c d
      | _ -> failwith "let")
  | IFTE (cmds1, cmds2) :: c -> (
      match s with
      | VInt i :: s ->
        if 0 < i then
          secd s e (cmds1 @ c) d
        else
          secd s e (cmds2 @ c) d
      | _ -> failwith "ifte")
  | [] -> (
      match s, d with
      | v :: c, (s', e', c') :: d' ->
        secd (v :: s') e' c' d'
      | [ v ], [] -> v
      | _ -> failwith "ret")
