let is_lower_case c = 'a' <= c && c <= 'z'

let is_upper_case c = 'A' <= c && c <= 'Z'

let is_alpha c = is_lower_case c || is_upper_case c

let is_digit c = '0' <= c && c <= '9'

let is_alphanum c = is_lower_case c || is_upper_case c || is_digit c

let is_blank c = String.contains " \012\n\r\t" c

let explode s = List.of_seq (String.to_seq s)

let implode ls = String.of_seq (List.to_seq ls)

type 'a parser =
  (char Seq.t -> ('a * char Seq.t) option) Lazy.t

let return (x : 'a) : 'a parser =
  lazy (fun cs -> Some (x, cs))

let fail : 'a parser =
  lazy (fun cs -> None)

let bind (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
  lazy
    (fun cs ->
       match (Lazy.force p) cs with
       | Some (a, cs) -> (Lazy.force (f a)) cs
       | _ -> None)

let (>>=) = bind

let (let*) = bind

let run_parser (p : 'a parser) (s : string) : ('a * string) option =
  let cs = String.to_seq s in
  match (Lazy.force p) cs with
  | Some (a, cs) -> Some (a, String.of_seq cs)
  | _ -> None

let read : char parser =
  lazy
    (fun cs ->
       match Seq.uncons cs with
       | Some (c, cs) -> Some (c, cs)
       | _ -> None)

let alt (p : 'a parser) (q : 'a parser) : 'a parser =
  lazy
    (fun cs ->
       match (Lazy.force p)  cs with
       | Some (a, cs) -> Some (a, cs)
       | _ -> (Lazy.force q) cs)

let (<|>) = alt

let choice (ps : 'a parser list) : 'a parser =
  List.fold_left (fun acc p -> p <|> acc) fail ps

let satisfy (f : char -> bool) : char parser =
  let* c = read in
  if f c then
    return c
  else
    fail

let seql (p : 'a parser) (q : 'b parser) : 'a parser =
  let* a = p in
  let* _ = q in
  return a

let (<<) = seql

let seqr (p : 'a parser) (q : 'b parser) : 'b parser =
  let* _ = p in
  let* a = q in
  return a

let (>>) = seqr

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  let* a = p in
  return (f a)

let (>|=) = map

let (>|) p c = p >> return c

let rec many (p : 'a parser) : 'a list parser =
  (let* x = p in
   let* xs = many p in
   return (x :: xs))
  <|>
  (return [])

let many1 (p : 'a parser) : 'a list parser =
  let* x = p in
  let* xs = many p in
  return (x :: xs)

let chainl (p : 'a parser) (q : ('a -> 'a -> 'a) parser) : 'a parser =
  let* x = p in
  let* fxs = many
      (let* f = q in
       let* x = p in
       return (f, x))
  in
  return (List.fold_left (fun acc (f, x) -> f acc x) x fxs)

let rec chainr (p : 'a parser) (q : ('a -> 'a -> 'a) parser) : 'a parser =
  let* x = p in
  (let* f = q in
   let* y = chainr p q in
   return (f x y))
  <|> return x

let blank : unit parser = satisfy is_blank >| ()

let ws : unit parser = many blank >| ()

let ws1 : unit parser = many1 blank >| ()

let digit : char parser = satisfy is_digit

let nat : int parser =
  let* xs = many1 digit in
  try
    return (int_of_string (implode xs))
  with _ -> fail

let char (c : char) : char parser =
  satisfy (Char.equal c)

let string (s : string) : unit parser =
  let ps = s |> explode |> List.map char in
  List.fold_right (>>) ps (return ())

let kw (s : string) : unit parser = string s >> ws >| ()

type expr =
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Var of string
  | Fun of string * string * expr
  | Ifte  of expr * expr * expr
  | Let of string * expr * expr
  | App of expr * expr
[@@deriving show {with_path = false}]

let reserved = ["fun"; "let"; "rec"; "in"; "if"; "then"; "else"]

let parens p = kw "(" >> p << kw ")"

let int_parser = nat << ws

let var_parser =
  let* c = satisfy is_alpha in
  let* cs = many (satisfy is_alphanum <|> char '_' <|> char '\'') in
  let s = implode (c :: cs) in
  if List.exists (String.equal s) reserved then
    fail
  else
    return s << ws

let rec fun_parser () =
  let* _ = kw "fun" in
  let* xs = many1 var_parser in
  let* _ = kw "->" in
  let* m = expr_parser () in
  let m =
    List.fold_right
      (fun x acc -> Fun ("", x, acc)) xs m
  in
  return m

and let_parser () =
  let* _ = kw "let" in
  let* x = var_parser in
  let* args = many var_parser in
  let* _ = kw "=" in
  let* m = expr_parser () in
  let* _ = kw "in" in
  let* n = expr_parser () in
  let m =
    List.fold_right
      (fun arg acc -> Fun ("", arg, acc)) args m
  in
  return (Let (x, m, n))

and letrec_parser () =
  let* _ = kw "let" in
  let* _ = kw "rec" in
  let* f = var_parser in
  let* x = var_parser in
  let* args = many var_parser in
  let* _ = kw "=" in
  let* m = expr_parser () in
  let* _ = kw "in" in
  let* n = expr_parser () in
  let m =
    List.fold_right
      (fun arg acc -> Fun ("", arg, acc)) args m
  in
  return (Let (f, Fun (f, x, m), n))

and ifte_parser () =
  let* _ = kw "if" in
  let* cond = expr_parser () in
  let* _ = kw "then" in
  let* m = expr_parser () in
  let* _ = kw "else" in
  let* n = expr_parser () in
  return (Ifte (cond, m, n))

and expr0_parser () =
  let* _ = return () in
  choice
    [ (int_parser >|= fun i -> Int i)
    ; (var_parser >|= fun x -> Var x)
    ; fun_parser ()
    ; letrec_parser ()
    ; let_parser ()
    ; ifte_parser ()
    ; parens (expr_parser ())
    ]

and expr1_parser () =
  let* _ = return () in
  chainl (expr0_parser ()) (return (fun x y -> App (x, y)))

and expr2_parser () =
  let* _ = return () in
  let opr =
    (kw "*" >| fun x y -> Mul (x, y)) <|>
    (kw "/" >| fun x y -> Div (x, y))
  in
  chainl (expr1_parser ()) opr

and expr_parser () =
  let* _ = return () in
  let opr =
    (kw "+" >| fun x y -> Add (x, y)) <|>
    (kw "-" >| fun x y -> Sub (x, y))
  in
  chainl (expr2_parser ()) opr
