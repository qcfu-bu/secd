module Ctx = Map.Make (String)
open Bindlib
open Ast

type expr =
  | EInt of int
  | EAdd of expr * expr
  | ESub of expr * expr
  | EMul of expr * expr
  | EDiv of expr * expr
  | EVar of expr var
  | EFun of (expr, expr) mbinder
  | EIfte of expr * expr * expr
  | ELet of expr * (expr, expr) binder
  | EApp of expr * expr

let mk_var s = new_var (fun x -> EVar x) s
let _EInt i = box (EInt i)
let _EAdd = box_apply2 (fun m n -> EAdd (m, n))
let _ESub = box_apply2 (fun m n -> ESub (m, n))
let _EMul = box_apply2 (fun m n -> EMul (m, n))
let _EDiv = box_apply2 (fun m n -> EDiv (m, n))
let _EVar : expr var -> expr box = box_var 
let _EFun = box_apply (fun bnd -> EFun bnd)
let _EIfte = box_apply3 (fun m n1 n2 -> EIfte (m, n1, n2))
let _ELet = box_apply2 (fun m bnd -> ELet (m, bnd))
let _EApp = box_apply2 (fun m n -> EApp (m, n))

let trans m =
  let rec trans ctx = function
    | Int i -> _EInt i
    | Add (m, n) -> _EAdd (trans ctx m) (trans ctx n)
    | Sub (m, n) -> _ESub (trans ctx m) (trans ctx n)
    | Mul (m, n) -> _EMul (trans ctx m) (trans ctx n)
    | Div (m, n) -> _EDiv (trans ctx m) (trans ctx n)
    | Var s -> _EVar (Ctx.find s ctx)
    | Fun (f, x, m) ->
      let f_ = mk_var f in
      let x_ = mk_var x in
      let ctx = Ctx.add f f_ ctx in
      let ctx = Ctx.add x x_ ctx in
      let m = trans ctx m in
      let bnd = bind_mvar [| f_; x_ |] m in
      _EFun bnd
    | Ifte (m, n1, n2) ->
      _EIfte (trans ctx m) (trans ctx n1) (trans ctx n2)
    | Let (x, m, n) ->
      let x_ = mk_var x in
      let m = trans ctx m in
      let n = trans (Ctx.add x x_ ctx) n in
      let bnd = bind_var x_ n in
      _ELet m bnd
    | App (m, n) -> _EApp (trans ctx m) (trans ctx n)
  in
  unbox (trans Ctx.empty m)

let rec interp m0 = 
  match m0 with
  | EInt _ -> m0
  | EVar _ -> failwith "var"
  | EAdd (m, n) -> (
    match interp m, interp n with
    | EInt x, EInt y -> EInt (x + y) 
    | _ -> failwith "add")
  | ESub (m, n) -> (
    match interp m, interp n with
    | EInt x, EInt y -> EInt (x - y) 
    | _ -> failwith "sub")
  | EMul (m, n) -> (
    match interp m, interp n with
    | EInt x, EInt y -> EInt (x * y) 
    | _ -> failwith "mul")
  | EDiv (m, n) -> (
    match interp m, interp n with
    | EInt x, EInt y -> EInt (x / y) 
    | _ -> failwith "div")
  | EFun _ -> m0
  | EApp (m, n) -> (
    match interp m, interp n with
    | (EFun bnd as f), v ->
      interp (msubst bnd [| f; v |])
    | _ -> failwith "app")
  | ELet (m, bnd) ->
    let v = interp m in
    interp (subst bnd v)
  | EIfte (m, n1, n2) -> (
    match interp m with
    | EInt i ->
      if 0 < i then interp n1 else interp n2
    | _ -> failwith "ifte")
