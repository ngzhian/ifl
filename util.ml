module Heap =
struct
  type addr = int
  let init = [], 0
  let alloc (heap, n_alloc) (node : 'a) =
    ((heap @ [node], n_alloc + 1), List.length heap)
  (* copy a node from addr m to addr n *)
  let copy ((heap, n_alloc)) m n =
    (List.mapi (fun i a -> if i = n then List.nth heap m else a) heap, n_alloc)
  (* update a node from at addr m to node *)
  let update ((heap, n_alloc)) m node =
    (List.mapi (fun i a -> if i = m then node else a) heap, n_alloc)
  let lookup (heap, n_alloc) (addr : addr) =
    List.nth heap addr
  let size heap =
    List.length heap
  let null = -1
end

(** Some helper functions for working with Lists *)
(* get the last element of ls *)
let last ls = List.nth ls (List.length ls - 1)

(* take n elements from ls, returning 2 lists *)
let rec take n ls = match (n, ls) with
  | 0, ls    -> [], ls
  | n, l::ls -> let (xs, ys) = take (n - 1) ls in l :: xs, ys
  | _, []    -> [], []

let drop n xs = snd (take n xs)

(* Maps a list by calling f on the acc and elements of the list
 * returning the final acc and the input list transformed by f *)
let rec map_accuml f acc xs = match xs with
  | []      -> (acc, [])
  | x :: xs -> let (acc1, y) = f acc x in
               let (acc2, ys) = map_accuml f acc1 xs in
               (acc2, y :: ys)
  (* possibly a faster impl *)
  (* let rec go acc xs ys = match xs with *)
  (*   | [] -> acc, ys *)
  (*   | x::xs -> let (acc', y) = f acc x in go acc' xs (y::ys) in *)
  (* let (acc, ys) = go acc xs [] in *)
  (* (acc, List.rev ys) *)

(* Create an assoc list with the in the second position *)
let indexify xs = List.mapi (fun i a -> (a, i)) xs
