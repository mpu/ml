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

  let memfix f =
    let m = Hashtbl.create 10 in
    let rec fix n =
      try Hashtbl.find m n.id
      with Not_found ->
        let res = f fix n in
        Hashtbl.add m n.id res;
        res
    in
    fix

  (* evaluation with memoization *)
  let evalm env =
    memfix (fun f -> evaln f env)

  (* extend the computation graph
     to compute the differential
     of the argument node and
     return it *)
  let fwd_grad =
    let go go n =
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
    memfix go

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

  let run ~memo env g gname =
    let eval =
      if memo
      then CG.evalm
      else CG.eval
    in
    epf "%s(%a) = %f@."
      gname pp_e env (eval env g);
    epf "Stats, %s memoization: %a@."
      (if memo then "with" else "no")
      Stat.dump CG.st;
    Stat.reset CG.st
  in

  let env = SMap.singleton "x" 3. in

  run ~memo:false env g1 "g1";
  run ~memo:true env g1 "g1";

  let dg1 = CG.fwd_grad g1 in
  epf "Grad keys: @[<h>%a@]@."
    pp_ls (Seq.map fst @@ SMap.to_seq dg1);
  
  let env = SMap.of_seq @@
    List.to_seq ["x", 2.; "x'", 1.] in
  let dg1x = SMap.find "x'" dg1 in

  run ~memo:false env dg1x "dg1x";
  run ~memo:true env dg1x "dg1x";

  let g2 =
    let open CG in
    let x0 = mk (Par "x") in
    let x1 = mk (Mul (x0, x0)) in
    let x2 = mk (Mul (x1, x1)) in
    let x3 = mk (Mul (x2, x2)) in
    let x4 = mk (Mul (x3, x3)) in
    let x5 = mk (Mul (x4, x4)) in
    let x6 = mk (Mul (x5, x5)) in
    let x7 = mk (Mul (x6, x6)) in
    x7
  in

  let dg2 = CG.fwd_grad g2 in
  let dg2x = SMap.find "x'" dg2 in

  run ~memo:false env g2 "g2";
  run ~memo:false env dg2x "dg2x";
  run ~memo:true env dg2x "dg2x";
  ()
