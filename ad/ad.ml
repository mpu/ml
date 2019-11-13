module SMap = Map.Make(String)

module Fresh = struct
  let i =
    let cnt = ref 0 in
    fun () ->
      incr cnt;
      !cnt
end

let pp_w fmt () =
  Format.fprintf fmt "@ "

let rec pp_l_ pp_s pp fmt seq =
  match seq () with
  | Seq.Nil -> ()
  | Seq.Cons (x, l)
    when l () = Seq.Nil ->
    pp fmt x
  | Seq.Cons (x, l) ->
    Format.fprintf fmt "%a%a%a"
      pp x pp_s () (pp_l_ pp_s pp) l

let pp_l pp = pp_l_ pp_w pp

let pp_ls = pp_l Format.pp_print_string

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

  (* extend the computation graph
     to compute the differential
     of the argument node and
     return it *)
  let fwd_grad n =
    let rec go n =
      match n.op with
      | Con _ ->
        SMap.empty
      | Par x ->
        let x' = x^"'" in
        SMap.singleton x' (mk (Par x'))
      | Add (n1, n2) ->
        let comb _ o1 o2 =
          match o1,o2 with
          | Some d1, Some d2 ->
            Some (mk (Add (d1, d2)))
          | Some d, None
          | None, Some d ->
            Some d
          | None, None ->
            None
        in
        SMap.merge comb
          (go n1)
          (go n2)
      | Mul (n1, n2) ->
        let comb _ o1 o2 =
          match o1,o2 with
          | Some d1, Some d2 ->
            let m1 = mk (Mul (d1, n2)) in
            let m2 = mk (Mul (n1, d2)) in
            Some (mk (Add (m1, m2)))
          | Some d1, None ->
            Some (mk (Mul (d1, n2)))
          | None, Some d2 ->
            Some (mk (Mul (n1, d2)))
          | None, None ->
            None
        in
        SMap.merge comb
          (go n1)
          (go n2)
    in
    go n

end

let _ =
  let g1 =
    let open CG in
    let x   = mk (Par "x") in
    let x2  = mk (Mul (x, x)) in
    let x3  = mk (Mul (x2, x)) in
    let tx3 = mk (Mul (mk (Con 2.), x3)) in
    let y   = mk (Add (tx3, x2)) in
    (* 2 x^3 + x^2 *)
    y
  in
  let pp_e fmt e =
    let pp_bnd fmt (a, b) =
      Format.fprintf fmt "%s=%g" a b
    in
    Format.fprintf fmt "@[<h>%a@]"
      (pp_l pp_bnd) (SMap.to_seq e)
  in
  let epf = Format.eprintf in
  let env = SMap.singleton "x" 3. in

  epf "g1(%a) = %f@."
    pp_e env (CG.eval env g1);
  epf "Stats, no memoization: %a@."
    Stat.dump CG.st;
  Stat.reset CG.st;

  epf "g1(%a) = %f@."
    pp_e env (CG.evalm env g1);
  epf "Stats, with memoization: %a@."
    Stat.dump CG.st;
  Stat.reset CG.st;

  let dg1 = CG.fwd_grad g1 in
  epf "Grad keys: @[<h>%a@]@."
    pp_ls (Seq.map fst @@ SMap.to_seq dg1);
  
  let env = SMap.of_seq @@
    List.to_seq ["x", 2.; "x'", 1.] in
  let dg1x = SMap.find "x'" dg1 in
  epf "dg1(%a) = %f@."
    pp_e env (CG.evalm env dg1x)

