module Env = Map.Make(Int)
open Ast

type hexpr =
  | HInt of int
  | HAdd of hexpr * hexpr
  | HSub of hexpr * hexpr
  | HMul of hexpr * hexpr
  | HDiv of hexpr * hexpr
  | HVar of int
  | HFun of int * int * hexpr
  | HIfte of hexpr * hexpr * hexpr
  | HLet of int * hexpr * hexpr
  | HApp of hexpr * hexpr
[@@deriving show {with_path = false}]

type value =
  | VInt of int
  | VClo of int * int * hexpr * (value Env.t[@opaque])
[@@deriving show {with_path = false}]

let env0 = Env.empty

let rec compile m =
  match m with
  | Int i -> HInt i
  | Add (m, n) -> HAdd (compile m, compile n)
  | Sub (m, n) -> HSub (compile m, compile n)
  | Mul (m, n) -> HMul (compile m, compile n)
  | Div (m, n) -> HDiv (compile m, compile n)
  | Var x -> HVar (Hashtbl.hash x)
  | Fun (f, x, m) ->
    let f = Hashtbl.hash f in
    let x = Hashtbl.hash x in
    let m = compile m in
    HFun (f, x, m)
  | Ifte (cond, m, n) ->
    HIfte (compile cond, compile m, compile n)
  | Let (x, m, n) ->
    HLet (Hashtbl.hash x, compile m, compile n)
  | App (m, n) ->
    HApp (compile m, compile n)

let rec tree (e : value Env.t) m =
  match m with
  | HVar x -> Env.find x e
  | HInt i -> VInt i
  | HAdd (m, n) -> (
      match tree e m, tree e n with
      | VInt i, VInt j -> VInt (i + j)
      | _ -> failwith "add")
  | HSub (m, n) -> (
      match tree e m, tree e n with
      | VInt i, VInt j -> VInt (i - j)
      | _ -> failwith "sub")
  | HMul (m, n) -> (
      match tree e m, tree e n with
      | VInt i, VInt j -> VInt (i * j)
      | _ -> failwith "mul")
  | HDiv (m, n) -> (
      match tree e m, tree e n with
      | VInt i, VInt j -> VInt (i / j)
      | _ -> failwith "mul")
  | HFun (f, x, n) -> VClo (f, x, n, e)
  | HApp (m, n) -> (
      match tree e m, tree e n with
      | (VClo (f, x, m, e) as clo), v ->
        tree (Env.add x v (Env.add f clo e)) m
      | _ -> failwith "app")
  | HLet (x, m, n) ->
    let v = tree e m in
    tree (Env.add x v e) n
  | HIfte (cond, m, n) -> (
      match tree e cond with
      | VInt i ->
        if 0 < i then
          tree e m
        else
          tree e n
      | _ -> failwith "ifte")


