(*configuration start*)
let threshold = 1000
let endgame_threshold = 30

(*source: http://www.samsoft.org.uk/reversi/strategy.htm*)
(*
let val_corner = 99
let val_c = -8
let val_x = -24
let val_a = 8
let val_a1 = -4
let val_a2 = 7
let val_b = 6
let val_b1 = -3
let val_b2 = 4
let val_start = 0
*)

let val_corner = 99
let val_c = 0
let val_x = 0
let val_a = 8
let val_a1 = 2
let val_a2 = 7
let val_b = 6
let val_b1 = 3
let val_b2 = 4
let val_start = 1
(*Named Openings
 * source: http://www.samsoft.org.uk/reversi/openings.htm#openings *)

(*original list
C4c3	Diagonal Opening
C4c3D3c5B2	X-square Opening
C4c3D3c5B3	Snake/Peasant
C4c3D3c5B3f3	Lysons
C4c3D3c5B3f4B5b4C6d6F5	Pyramid/Checkerboarding Peasant
C4c3D3c5B4	Heath/Tobidashi
C4c3D3c5B4d2C2f4D6c6F5e6F7	Mimura Variation II
C4c3D3c5B4d2D6	Heath-Bat
C4c3D3c5B4d2E2	Iwasaki Variation
C4c3D3c5B4e3	Heath-Chimney
C4c3D3c5B5	Raccoon Dog
C4c3D3c5B6	Rocket
C4c3D3c5B6c6B5	Hamilton
C4c3D3c5B6e3	Lollipop
C4c3D3c5D6	Cow
C4c3D3c5D6e3	Chimney
C4c3D3c5D6f4B4	Cow Bat/Bat/Cambridge
C4c3D3c5D6f4B4b6B5c6B3	Bat (Piau Continuation 2)
C4c3D3c5D6f4B4b6B5c6F5	Melnikov/Bat (Piau Continuation 1)
C4c3D3c5D6f4B4c6B5b3B6e3C2a4A5a6D2	Bat (Kling Continuation)
C4c3D3c5D6f4B4e3B3	Bat (Kling Alternative)
C4c3D3c5D6f4F5	Rose-v-Toth
C4c3D3c5D6f4F5d2	Tanida
C4c3D3c5D6f4F5d2B5	Aircraft/Feldborg
C4c3D3c5D6f4F5d2G4d7	Sailboat
C4c3D3c5D6f4F5e6C6d7	Maruoka
C4c3D3c5D6f4F5e6F6	Landau
C4c3D3c5F6	Buffalo/Kenichi Variation
C4c3D3c5F6e2C6	Maruoka Buffalo
C4c3D3c5F6e3C6f5F4g5	Tanida Buffalo
C4c3D3c5F6f5	Hokuriku Buffalo
C4c3E6c5	Wing Variation
C4c3F5c5	Semi-Wing Variation
C4c5	Parallel Opening
C4e3	Perpendicular Opening
C4e3F4c5D6e6	Mimura
C4e3F4c5D6f3C6	Shaman/Danish
C4e3F4c5D6f3D3	Inoue
C4e3F4c5D6f3D3c3	Iago
C4e3F4c5D6f3E2	Bhagat
C4e3F4c5D6f3E6c3D3e2	Rose
C4e3F4c5D6f3E6c3D3e2B5	Flat
C4e3F4c5D6f3E6c3D3e2B5f5	Rotating Flat
C4e3F4c5D6f3E6c3D3e2B5f5B3	Murakami Variation
C4e3F4c5D6f3E6c3D3e2B5f5B4f6C2e7D2c7	Rotating Flat (Kling Continuation)
C4e3F4c5D6f3E6c3D3e2B6f5	Rose-Birth
C4e3F4c5D6f3E6c3D3e2B6f5B4f6G5d7	Brightstein
C4e3F4c5D6f3E6c3D3e2B6f5G5	Rose-Birdie/Rose-Tamenori
C4e3F4c5D6f3E6c3D3e2B6f5G5f6	Rose-Tamenori-Kling
C4e3F4c5D6f3E6c3D3e2D2	Greenberg/Dawg
C4e3F4c5D6f3E6c6	Ralle
C4e3F4c5E6	Horse
C4e3F5b4	Ganglion/No-Cat
C4e3F5b4F3	Swallow
C4e3F5b4F3f4E2e6G5f6D6c6	No-Cat (Continuation)
C4e3F5e6D3	Italian
C4e3F5e6F4	Cat
C4e3F5e6F4c5D6c6F7f3	Sakaguchi
C4e3F5e6F4c5D6c6F7g5G6	Berner
C4e3F6b4	Bent Ganglion
C4e3F6e6F5	Tiger
C4e3F6e6F5c5C3	Stephenson
C4e3F6e6F5c5C3b4	No-Kung
C4e3F6e6F5c5C3b4D6c6B5a6B6c7	No-Kung (Continuation)
C4e3F6e6F5c5C3c6	Comp'Oth
C4e3F6e6F5c5C3c6D3d2E2b3C1c2B4a3A5b5A6a4A2	F.A.T. Draw
C4e3F6e6F5c5C3c6D6	Lighning Bolt
C4e3F6e6F5c5C3g5	Kung
C4e3F6e6F5c5D3	Leader's Tiger
C4e3F6e6F5c5D6	Brightwell
C4e3F6e6F5c5F4g5G4f3C6d3D6	Ishii
C4e3F6e6F5c5F4g5G4f3C6d3D6b3C3b4E2b6	Mainline Tiger
C4e3F6e6F5c5F4g6F7	Rose-Bill
C4e3F6e6F5c5F4g6F7d3	Tamenori
C4e3F6e6F5c5F4g6F7g5	Central Rose-Bill/Dead Draw
C4e3F6e6F5g6	Aubrey/Tanaka
C4e3F6e6F5g6E7c5	Aubrey (Feldborg Continuation)
*)

(*black start*)
let openings = [
    [ (3, 4); (3, 3) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (2, 2) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (2, 3) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (2, 3); (6, 3) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (2, 3); (6, 4); (2, 5); (2, 4); (3, 6); (4, 6); (6, 5) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (2, 4) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (2, 4); (4, 2); (3, 2); (6, 4); (4, 6); (3, 6); (6, 5); (5, 6); (6, 7) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (2, 4); (4, 2); (4, 6) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (2, 4); (4, 2); (5, 2) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (2, 4); (5, 3) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (2, 5) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (2, 6) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (2, 6); (3, 6); (2, 5) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (2, 6); (5, 3) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (4, 6) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (4, 6); (5, 3) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (4, 6); (6, 4); (2, 4) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (4, 6); (6, 4); (2, 4); (2, 6); (2, 5); (3, 6); (2, 3) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (4, 6); (6, 4); (2, 4); (2, 6); (2, 5); (3, 6); (6, 5) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (4, 6); (6, 4); (2, 4); (3, 6); (2, 5); (2, 3); (2, 6); (5, 3); (3, 2); (1, 4); (1, 5); (1, 6); (4, 2) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (4, 6); (6, 4); (2, 4); (5, 3); (2, 3) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (4, 6); (6, 4); (6, 5) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (4, 6); (6, 4); (6, 5); (4, 2) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (4, 6); (6, 4); (6, 5); (4, 2); (2, 5) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (4, 6); (6, 4); (6, 5); (4, 2); (7, 4); (4, 7) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (4, 6); (6, 4); (6, 5); (5, 6); (3, 6); (4, 7) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (4, 6); (6, 4); (6, 5); (5, 6); (6, 6) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (6, 6) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (6, 6); (5, 2); (3, 6) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (6, 6); (5, 3); (3, 6); (6, 5); (6, 4); (7, 5) ];
    [ (3, 4); (3, 3); (4, 3); (3, 5); (6, 6); (6, 5) ];
    [ (3, 4); (3, 3); (5, 6); (3, 5) ];
    [ (3, 4); (3, 3); (6, 5); (3, 5) ];
    [ (3, 4); (3, 5) ];
    [ (3, 4); (5, 3) ];
    [ ( 3, 4 ); (5, 3); (6, 4); (3, 5); (4, 6); (5, 6) ];
    [ (3, 4); (5, 3); (6, 4); (3, 5); (4, 6); (6, 3); (3, 6) ];
    [ (3, 4); (5, 3); (6, 4); (3, 5); (4, 6); (6, 3); (4, 3) ];
    [ (3, 4); (5, 3); (6, 4); (3, 5); (4, 6); (6, 3); (4, 3); (3, 3) ];
    [ (3, 4); (5, 3); (6, 4); (3, 5); (4, 6); (6, 3); (5, 2) ];
    [ (3, 4); (5, 3); (6, 4); (3, 5); (4, 6); (6, 3); (5, 6); (3, 3); (4, 3); (5, 2) ];
    [ (3, 4); (5, 3); (6, 4); (3, 5); (4, 6); (6, 3); (5, 6); (3, 3); (4, 3); (5, 2); (2, 5) ];
    [ (3, 4); (5, 3); (6, 4); (3, 5); (4, 6); (6, 3); (5, 6); (3, 3); (4, 3); (5, 2); (2, 5); (6, 5) ];
    [ (3, 4); (5, 3); (6, 4); (3, 5); (4, 6); (6, 3); (5, 6); (3, 3); (4, 3); (5, 2); (2, 5); (6, 5); (2, 3) ];
    [ (3, 4); (5, 3); (6, 4); (3, 5); (4, 6); (6, 3); (5, 6); (3, 3); (4, 3); (5, 2); (2, 5); (6, 5); (2, 4); (6, 6); (3, 2); (5, 7); (4, 2); (3, 7) ];
    [ (3, 4); (5, 3); (6, 4); (3, 5); (4, 6); (6, 3); (5, 6); (3, 3); (4, 3); (5, 2); (2, 6); (6, 5) ];
    [ (3, 4); (5, 3); (6, 4); (3, 5); (4, 6); (6, 3); (5, 6); (3, 3); (4, 3); (5, 2); (2, 6); (6, 5); (2, 4); (6, 6); (7, 5); (4, 7) ];
    [ (3, 4); (5, 3); (6, 4); (3, 5); (4, 6); (6, 3); (5, 6); (3, 3); (4, 3); (5, 2); (2, 6); (6, 5); (7, 5) ];
    [ (3, 4); (5, 3); (6, 4); (3, 5); (4, 6); (6, 3); (5, 6); (3, 3); (4, 3); (5, 2); (2, 6); (6, 5); (7, 5); (6, 6) ];
    [ (3, 4); (5, 3); (6, 4); (3, 5); (4, 6); (6, 3); (5, 6); (3, 3); (4, 3); (5, 2); (4, 2) ];
    [ (3, 4); (5, 3); (6, 4); (3, 5); (4, 6); (6, 3); (5, 6); (3, 6) ];
    [ (3, 4); (5, 3); (6, 4); (3, 5); (5, 6) ];
    [ (3, 4); (5, 3); (6, 5); (2, 4) ];
    [ (3, 4); (5, 3); (6, 5); (2, 4); (6, 3) ];
    [ (3, 4); (5, 3); (6, 5); (2, 4); (6, 3); (6, 4); (5, 2); (5, 6); (7, 5); (6, 6); (4, 6); (3, 6) ];
    [ (3, 4); (5, 3); (6, 5); (5, 6); (4, 3) ];
    [ (3, 4); (5, 3); (6, 5); (5, 6); (6, 4) ];
    [ (3, 4); (5, 3); (6, 5); (5, 6); (6, 4); (3, 5); (4, 6); (3, 6); (6, 7); (6, 3) ];
    [ (3, 4); (5, 3); (6, 5); (5, 6); (6, 4); (3, 5); (4, 6); (3, 6); (6, 7); (7, 5); (7, 6) ];
    [ (3, 4); (5, 3); (6, 6); (2, 4) ];
    [ (3, 4); (5, 3); (6, 6); (5, 6); (6, 5) ];
    [ (3, 4); (5, 3); (6, 6); (5, 6); (6, 5); (3, 5); (3, 3) ];
    [ (3, 4); (5, 3); (6, 6); (5, 6); (6, 5); (3, 5); (3, 3); (2, 4) ];
    [ (3, 4); (5, 3); (6, 6); (5, 6); (6, 5); (3, 5); (3, 3); (2, 4); (4, 6); (3, 6); (2, 5); (1, 6); (2, 6); (3, 7) ];
    [ (3, 4); (5, 3); (6, 6); (5, 6); (6, 5); (3, 5); (3, 3); (3, 6) ];
    [ (3, 4); (5, 3); (6, 6); (5, 6); (6, 5); (3, 5); (3, 3); (3, 6); (4, 3); (4, 2); (5, 2); (2, 3); (3, 1); (3, 2); (2, 4); (1, 3); (1, 5); (2, 5); (1, 6); (1, 4); (1, 2) ];
    [ (3, 4); (5, 3); (6, 6); (5, 6); (6, 5); (3, 5); (3, 3); (3, 6); (4, 6) ];
    [ (3, 4); (5, 3); (6, 6); (5, 6); (6, 5); (3, 5); (3, 3); (7, 5) ];
    [ (3, 4); (5, 3); (6, 6); (5, 6); (6, 5); (3, 5); (4, 3) ];
    [ (3, 4); (5, 3); (6, 6); (5, 6); (6, 5); (3, 5); (4, 6) ];
    [ (3, 4); (5, 3); (6, 6); (5, 6); (6, 5); (3, 5); (6, 4); (7, 5); (7, 4); (6, 3); (3, 6); (4, 3); (4, 6) ];
    [ (3, 4); (5, 3); (6, 6); (5, 6); (6, 5); (3, 5); (6, 4); (7, 5); (7, 4); (6, 3); (3, 6); (4, 3); (4, 6); (2, 3); (3, 3); (2, 4); (5, 2); (2, 6) ];
    [ (3, 4); (5, 3); (6, 6); (5, 6); (6, 5); (3, 5); (6, 4); (7, 6); (6, 7) ];
    [ (3, 4); (5, 3); (6, 6); (5, 6); (6, 5); (3, 5); (6, 4); (7, 6); (6, 7); (4, 3) ];
    [ (3, 4); (5, 3); (6, 6); (5, 6); (6, 5); (3, 5); (6, 4); (7, 6); (6, 7); (7, 5) ];
    [ (3, 4); (5, 3); (6, 6); (5, 6); (6, 5); (7, 6) ];
    [ (3, 4); (5, 3); (6, 6); (5, 6); (6, 5); (7, 6); (5, 7); (3, 5) ]
]
(*configuration end*)

open Array
open Color
open Command
open Int64

(*const*)
type board_t = int64 * int64
let pos_corner = (1, 1)
let pos_c = (2, 1)
let pos_x = (2, 2)
let pos_a = (3, 1)
let pos_a1 = (3, 2)
let pos_a2 = (3, 3)
let pos_b = (4, 1)
let pos_b1 = (4, 2)
let pos_b2 = (4, 3)
let pos_start = (4, 4)

let val_max = val_corner * 64 + 1
let val_min = - val_max

let all_pos =
    let mix xs ys =
        List.concat (List.map (fun x -> List.map (fun y -> (x,y)) ys) xs)
    in
    let ls = [1;2;3;4;5;6;7;8] in
    (mix ls ls)
let board_empty = (zero, zero)

(**************BIT**************)
(*set bit on*)
let bit_set (num: int64) (pos: int) =
    logor num (shift_left one pos)

(*get bit*)
let bit_get (num: int64) (pos: int) =
    (logand (shift_right_logical num pos) one) = one

let bit_clear (num: int64) (pos: int) =
    logand (lognot (shift_left one pos)) num

(****************POS***************)
(*pos encode and decode*)
let pos_encode (x, y) =
    (x - 1) * 8 + y - 1

let pos_decode (pos: int) = pos / 8 + 1, pos mod 8 + 1
let pos_rotate_one (x, y) = 9 - y, x
let pos_flip (x, y) = y, x
let pos_sym_encode (x, y) =
    let mx, my = Pervasives.min x (9 - x), Pervasives.min y (9 - y) in
        pos_encode (Pervasives.max mx my, Pervasives.min mx my)
(*move by black*)
let board_set_black ((bb, bw) : board_t) pos =
    let pp = pos_encode pos in
        bit_set bb pp, bit_clear bw pp

let board_set_white (bb, bw) pos =
    let pp = pos_encode pos in
        bit_clear bb pp, bit_set bw pp

let board_get_color ((bb, bw) : board_t) (x, y) =
    if x == 0 || x == 9 || y == 0 || y == 9 then
        sentinel
    else
        let ipos = pos_encode (x, y) in
        if bit_get bb ipos then
            black
        else
            if bit_get bw ipos then
                white
            else
                none

let board_rotate_one board =
    List.fold_left (fun bb pos ->
        let color =  board_get_color board pos in
        let pp = pos_rotate_one pos in
            if color = black then
                board_set_black bb pp
            else
                if color = white then
                    board_set_white bb pp
                else
                    bb) board_empty all_pos

let board_flipped board =
    List.fold_left (fun bb pos ->
        let color = board_get_color board pos in
        let pp = pos_flip pos in
            if color = black then
                board_set_black bb pp
            else
                if color = white then
                    board_set_white bb pp
                else
                    bb
    ) board_empty all_pos
(*tail recursive*)
let board_rotated (board: board_t) =
    Pervasives.fst (List.fold_left (fun (blist, bb) _ -> (bb::blist, board_rotate_one bb)) ([], board) [1;1;1;1])

let board_swap (bb, bw) = (bw, bb)

(*init board*)
let board_init () =
    board_set_black
        (board_set_black
            (board_set_white
                (board_set_white board_empty (4, 4))
                (5, 5))
            (4, 5))
        (5, 4)
let board_encode (board: board_t) =
    List.fold_left (fun current iter -> Pervasives.min current iter) board ((board_rotated board) @ (board_rotated (board_flipped board)))
module Board = struct
    type t = board_t
    let compare (x1, y1) (x2, y2) =
        if x1 != x2 then
            compare x1 x2
        else
            compare y1 y2
end
module Minimax = Map.Make(Board)
module OpeningMap = Map.Make(Board)
module PosMap = Map.Make (struct type t = int * int let compare p1 p2 =
    Pervasives.compare (pos_sym_encode p1) (pos_sym_encode p2)
    end)
let pos_val_map =
    let pos_list = [pos_corner; pos_c; pos_x; pos_a; pos_a1; pos_a2; pos_b; pos_b1; pos_b2; pos_start] in
    let val_list = [val_corner; val_c; val_x; val_a; val_a1; val_a2; val_b; val_b1; val_b2; val_start] in
    List.fold_left (fun map (pos, value) -> PosMap.add pos value map ) PosMap.empty (List.map2 (fun pos value -> (pos, value)) pos_list val_list)

let pos_val pos = PosMap.find pos pos_val_map

(*******************BOARD*******************)

let dirs = [ (-1,-1); (0,-1); (1,-1); (-1,0); (1,0); (-1,1); (0,1); (1,1) ]

let flippable_indices_line board (di,dj) (i,j) =
    let rec g (di,dj) (i,j) r =
        if board_get_color board (i, j) = white then
            g (di,dj) (i+di,j+dj) ( (i,j) :: r )
        else if board_get_color board (i, j) = black then
            r
        else
            []
    in
    let f (di,dj) (i,j) r =
        if (board_get_color board (i, j)) = white then
            g (di,dj) (i+di,j+dj) ( (i,j) :: r )
        else
            []
    in
    f (di,dj) (i,j) []

let flippable_indices board (i,j) =
  let bs = List.map (fun (di,dj) -> flippable_indices_line board (di,dj) (i+di,j+dj)) dirs in
    List.concat bs

let is_effective board pos =
  match flippable_indices board pos with
      [] -> false
    | _  -> true

let is_valid_move board pos =
  ((board_get_color board pos) = none) && is_effective board pos

let board_do_move board com =
  match com with
      GiveUp  -> board
    | Pass    -> board
    | Mv (x, y) ->
        let ms = flippable_indices board (x, y) in
            board_set_black (List.fold_left (fun bb pp -> board_set_black bb pp) board ms) (x, y)
let doMove board com color =
    if color = black then
        board_do_move board com
    else
        board_swap (board_do_move (board_swap board) com)
let minimax_move board pos =
    board_swap (board_do_move board (Mv (Pervasives.fst pos, Pervasives.snd pos)))

(*always started by black*)
(* map board to list of board*)
let opening_map =
    let opening_map_add board next_board map =
        let current_list =
            if OpeningMap.mem board map then
                OpeningMap.find board map
            else
                []
        in
        OpeningMap.add board (next_board::current_list) map
    in
    List.fold_left (fun map moves_list ->
        Pervasives.fst (List.fold_left (fun (mm, board) pos ->
            if is_valid_move board pos then
                let bb = minimax_move board pos in
                    opening_map_add board bb mm, bb
            else
                mm, board
            ) (map, board_init ()) moves_list
        )
    ) OpeningMap.empty openings

let board_refine board color =
    if color = white then board_swap board else board

let valid_moves board =
  List.filter (is_valid_move board) all_pos

let count board color =
  let s = ref 0 in
    for i=1 to 8 do
      for j=1 to 8 do
        if (board_get_color board (i, j)) = color then s := !s + 1
      done
    done;
    !s

let board_is_end_game board =
    (count board black) + (count board white) >= endgame_threshold

let board_eval_core board =
    let is_end_game = board_is_end_game board in
        List.fold_left (fun v pos ->
            if board_get_color board pos = black then
                v + (if is_end_game then 1 else pos_val pos)
            else
                v
           ) 0 all_pos

let board_eval board =
    (board_eval_core board) - (board_eval_core (board_swap board))

(*use bfs to search*)
(*tail recursive*)
let rec minimax_expand map queue board_list cnt =
    if cnt >= threshold || Queue.is_empty queue then
        map, board_list
    else
        let board = Queue.pop queue in
        let mm, bbl, cc =
            List.fold_left (fun (mm, bbl, cc) pos ->
                let bb = minimax_move board pos in
                    if Minimax.mem bb mm then
                        mm, bbl, cc
                    else
                        let _ = Queue.push bb queue in
                        let mmm = Minimax.add bb (board_eval bb) mm in
                        let bbbl = bb::bbl in
                            mmm, bbbl, cc + 1
            ) (map, board_list, cnt) (valid_moves board)
        in
            minimax_expand mm queue bbl cc

(*return a map that maps all next moves to theirs min, max value*)
let board_minimax board =
    let init_queue = Queue.create () in
    let _ = Queue.push board init_queue in
    let init_map = Minimax.add board (board_eval board) Minimax.empty in
    (*let start = Sys.time () in*)
    let init_map', board_list = minimax_expand init_map init_queue [] 1 in
    (*let _ = Printf.printf "expand time: %d %d %f s\n" (Minimax.cardinal init_map') (List.length board_list) (Sys.time() -. start) in*)
    (*let start = Sys.time () in*)
    let t =
        List.fold_left (fun map bb ->
            let bcount, wcount = count bb black, count bb white in
            if bcount + wcount = 64 then (*end game*)
                if bcount > wcount then
                    Minimax.add bb val_max map (*win*)
                else
                    if bcount = wcount then
                        Minimax.add bb 0 map (*tie*)
                    else
                        Minimax.add bb val_min map (*lose*)
            else
                List.fold_left (fun mm pos ->
                    let after_move = minimax_move bb pos in
                    let new_max =
                        if Minimax.mem after_move mm then
                            Minimax.find after_move mm
                        else
                            board_eval after_move
                    in
                    let new_max = -new_max in
                    let old_max = Minimax.find bb mm in
                        if new_max > old_max then
                            Minimax.add bb (Pervasives.max new_max old_max) mm
                        else
                            mm
                ) map (valid_moves bb)
    ) init_map' board_list
    in
    (*let _ = Printf.printf "minimax time: %f s\n" (Sys.time() -. start) in*)
    t

let play board' color =
    let board = board_refine board' color in
    let ms =
        if OpeningMap.mem board opening_map then
            match OpeningMap.find board opening_map with
            bb::_ ->
                List.fold_left (fun current pos ->
                    if board_encode (minimax_move board pos) = board_encode bb then
                        pos::current
                    else
                        current
                ) [] all_pos
            | _ -> []
        else
            []
    in
    let ms =
        if ms = [] then
            let minimax = board_minimax board in
                List.fold_left (fun current pos ->
                    match current with
                    [] -> [pos]
                    | first::_ ->
                        let current_max = Minimax.find (minimax_move board first) minimax in
                        let iter_max =  Minimax.find (minimax_move board pos) minimax in
                        if current_max = iter_max then
                            pos :: current
                        else
                            if current_max < iter_max then
                                current
                            else
                                [pos]
                ) [] (valid_moves board)
        else
            ms
    in
    let ms =
        if ms = [] then
            valid_moves board
        else
            ms
    in
    if ms = [] then
      Pass
    else
        let k = Random.int (List.length ms) in
        let (i,j) = List.nth ms k in
            Mv (i,j)

let print_board board =
  print_endline " |A B C D E F G H ";
  print_endline "-+----------------";
  for j=1 to 8 do
    print_int j; print_string "|";
    for i=1 to 8 do
      print_color (board_get_color board (i, j)); print_string " "
    done;
    print_endline ""
  done;
  print_endline "  (X: Black,  O: White)"

let report_result board =
  let _ = print_endline "========== Final Result ==========" in
  let bc = count board black in
  let wc = count board white in
    if bc > wc then
      print_endline "*Black wins!*"
    else if bc < wc then
      print_endline "*White wins!*"
    else
      print_endline "*Even*";
    print_string "Black: "; print_endline (string_of_int bc);
    print_string "White: "; print_endline (string_of_int wc);
    print_board board

(*for debug*)
let (>>) board ((x, y), color) =
    doMove board (Mv (x, y)) color

    (*
let _ =
let board = board_init () in
let _ = print_board board in
let ms = flippable_indices board (4, 3) in
let board = board >> ((5, 6), black) >> ((6, 4), white) in
let _ = print_board board in
let board = board_swap board in
let _ = print_board board in
let board = board_swap board in
let x , y = board in
Printf.printf "%s %s\n" (to_string x) (to_string y)
*)
(*
        11 1000 0001 0000 0000 0000 0000 0000 0000 0000
 1000 0001 1000 0000 1000 0000 0000 0000 0000 0000 0000
 * *)
