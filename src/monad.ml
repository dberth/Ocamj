module type MONAD =
  sig
    type 'a m
    val return: 'a -> 'a m
    val bind: 'a m -> ('a -> 'b m) -> 'b m
  end

module Monad(M: MONAD) =
  struct
    include M

    let seq m f = bind m (fun _ -> f)

    let join m = bind m (fun m -> m)

    let fmap f m = bind m (fun x -> return (f x))

    let liftm = fmap

    let liftm2 f m m' =
      bind m (fun x ->
	bind m' (fun y ->
	  return (f x y)))

    let liftm3 f m m' m'' =
      bind m (fun x ->
	bind m' (fun y ->
	  bind m'' (fun z ->
	    return (f x y z))))

    let mapm f l =
      List.fold_right (liftm2 (fun x xs -> f x :: xs)) l (return [])

    let sequence l = mapm (fun x -> x) l

    let mapm_ f l =
      List.fold_right (fun x -> seq (f x)) l (return ())

    let sequence_ l = mapm_ (fun x -> x) l

    module Op =
      struct
	let (>>=) = bind
	let (>>) = seq
      end
  end

module State(S: sig type t end) =
  struct
    type 'a m = St of (S.t -> 'a * S.t)

    let return x = St(fun st -> x, st)

    let bind (St m) f =
      St
	(fun st ->
	  let (x, st') = m st in
	  let (St m') = f x in
	  m' st'
	)
    
    let get = St (fun st -> st, st)
    let set st = St (fun _ -> (), st)

    let eval (St m) st = fst (m st)

    let run (St m) st = snd (m st)
  end

module Lwt_state(S: sig type t end) =
  struct
    type 'a m = St of (S.t option ->'a Lwt.t)

    let state = Lwt.new_key ()

    let return x = St (fun st -> Lwt.with_value state st (fun () -> Lwt.return x))

    let bind (St m) f =
      St
	(fun st ->
	   let thread = m st in
	   let st' = Lwt.get state in
	   let m' = Lwt.with_value state st' (fun () -> (fun st -> Lwt.bind thread (fun x -> let St thread = f x in thread st))) in
	     m' st'
	)

    let get = St (fun st -> Lwt.return st)

    let set st = St (fun _ -> Lwt.with_value state st (fun () -> Lwt.return ()))

    let run (St m) st = m st
  end
