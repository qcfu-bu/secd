open Secd
open Ast
open Parser

type mode =
  | TREE
  | SUBST
  | SECD_OLD
  | SECD_NEW

let interp mode s =
  match run_parser (ws >> expr_parser ()) s with
  | Some (m, "") -> (
      match mode with
      | TREE -> Tree.(
          let hm = compile m in
          Fmt.str "%a" pp_value (tree env0 hm))
      | SUBST -> Subst.(
          let m = trans m in
          match interp m with
          | EInt i -> Fmt.str "%d" i
          | EFun _ -> Fmt.str "<fun>"
          | _ -> failwith "eval_subst")
      | SECD_OLD -> Secd_old.(
          let cmds = compile m in
          let res = secd [] env0 cmds [] in
          Fmt.str "%a" pp_value res)
      | SECD_NEW -> Secd_new.(
          let cmds = compile [] m in
          let res = secd [] [] cmds in
          Fmt.str "%a" pp_value res))
  | _ -> failwith "parse error"

let test0 =
  "let add = fun x -> fun y -> x in
   add (fun z -> z)"

let test1 =
  "let add = fun x -> fun y -> x + y in
   let addx = add (1 + 2) in
   addx 2"

(* tree     : 0.96s  *)
(* secd_old : 1.05s *)
(* secd_new : 0.28s *)
let test2 =
  "let zero f x = x in
   let succ n f x = f (n f x) in
   let add n m f x = n f (m f x) in
   let mul n m f x = n (m f) x in
   let one = succ zero in
   let two = succ one in
   let three = succ two in
   let nine = mul three three in
   let ten = succ nine in
   let hundred = mul ten ten in
   let fact k =
     k (fun p -> p (fun a b g -> g (fun f x -> f (a f x)) (fun f -> a (b f))))
       (fun g -> g (fun h -> h) (fun h -> h)) (fun a b -> b)
   in
   let int_of_num n = n (fun x -> x + 1) 0 in
   int_of_num (fact ten)"

(* something interesting happens here *)
let test3 =
  "let omega = fun x -> x x in
   omega omega"

let test4 =
  "let x = 1 in
   let y =
      let x = 30 in
      x + 3
   in x"

(* tree     : seg fault *)
(* secd_old : 0.28s *)
(* secd_new : 0.25s *)
let test5 =
  "let rec fact n =
     if n then
       n * fact (n - 1)
     else
       1
   in fact 1000000"

let test6 =
  "let rec mccarthy n =
     if 101 - n then
       mccarthy (mccarthy (n + 11))
     else
       n - 10
   in mccarthy 33"

(* tree     : 15.83s *)
(* secd_old : 20.62s *)
(* secd_new : 16.75s *)
let test7 =
  "let rec fibo n =
      if n - 1 then
        fibo (n - 1) + fibo (n - 2)
      else
        n
   in fibo 40"

let test8 =
  "let fibo n =
     let rec loop i a b =
        if i then
          loop (i - 1) b (a + b)
        else
          a
     in loop n 0 1
   in fibo 40"

let _ = Fmt.pr "result := %s@." (interp SUBST test2)