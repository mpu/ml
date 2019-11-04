module SMap = Map.Make(String)

module Fresh = struct
  let i =
    let cnt = ref 0 in
    fun () ->
      incr cnt;
      !cnt
end

module Stat = struct
  type t = (string, int) Hashtbl.t

  let mk (): t =
    Hashtbl.create 10

  let reset st =
    Hashtbl.reset st

  let bump st ?(n=1) cnt =
    let c =
      try Hashtbl.find st cnt
      with Not_found -> 0
    in
    Hashtbl.replace st cnt (c + n)

  let dump fmt (st: t) =
    Hashtbl.iter
      (Format.fprintf fmt "%s: %d ")
      st
end

(* compute graph *)
module CG = struct
  type node = {
    id: int;
    op: op;
  }

  and op =
    | Par of string
    | Con of float
    | Add of node * node
    | Sub of node * node
    | Mul of node * node

  let st = Stat.mk ()

  let mk op =
    let id = Fresh.i () in
    {id = id; op = op}

  let evaln fix env n =
    match n.op with
    | Con f ->
      Stat.bump st "con";
      f
    | Par x ->
      Stat.bump st "par";
      SMap.find x env
    | Add (n1, n2) ->
      Stat.bump st "add";
      fix n1 +. fix n2
    | Sub (n1, n2) ->
      Stat.bump st "sub";
      fix n1 -. fix n2
    | Mul (n1, n2) ->
      Stat.bump st "mul";
      fix n1 *. fix n2

  (* this evaluation function does not
     benefit from the sharing in the
     computation graph *)
  let rec eval env n =
    evaln (eval env) env n

  (* evaluation with memoization *)
  let evalm env n =
    let m = Hashtbl.create 10 in
    let rec fix n =
      try Hashtbl.find m n.id
      with Not_found ->
        let res = evaln fix env n in
        Hashtbl.add m n.id res;
        res
    in
    fix n

end

let _ =
  let g1 =
    let open CG in
    let x   = mk (Par "x") in
    let x2  = mk (Mul (x, x)) in
    let x3  = mk (Mul (x2, x)) in
    let tx3 = mk (Mul (mk (Con 2.), x3)) in
    let y   = mk (Add (tx3, x2)) in
    y
  in
  let env = SMap.singleton "x" 3. in
  Format.eprintf "g1(x=3) = %f@."
    (CG.eval env g1);
  Format.eprintf "Stats, no memoization: %a@."
    Stat.dump CG.st;
  Stat.reset CG.st;
  Format.eprintf "g1(x=3) = %f@."
    (CG.evalm env g1);
  Format.eprintf "Stats, with memoization: %a@."
    Stat.dump CG.st
