type serie =
  | Dot
  | Bam
  | Char

type color =
  | White
  | Green
  | Red

type wind =
  | East
  | South
  | West
  | North

type tile_descr =
  | Serie of serie * int
  | Dragon of color
  | Wind of wind

type basic_tile_descr = tile_descr * int

type tile = int

let max_tile = 135

let tile_descrs = Hashtbl.create 136

let tiles =
  Array.init
    136
    (fun i ->
      let test limit tile_descr k =
        if i < limit then let result = tile_descr, i mod 4 in Hashtbl.add tile_descrs result i; result else k
      in
      test 36 (Serie(Dot, i / 4 + 1))
        (test 72 (Serie(Bam, (i - 36) / 4 + 1))
           (test 108 (Serie (Char, (i - 72) / 4 + 1))
              (test 112 (Dragon White)
                 (test 116 (Dragon Green)
                    (test 120 (Dragon Red)
                       (test 124 (Wind East)
                          (test 128 (Wind South)
                             (test 132 (Wind West)
                                (test 136 (Wind North) (Serie(Dot, 42), 0))
                             )
                          )
                       )
                    )
                 )
              )
           )
        )
    )

let tile_of_descr ?(instance = 0) descr =
  try
    Hashtbl.find tile_descrs (descr, instance)
  with
  | Not_found -> assert false

let descr_of_tile i = tiles.(i)

let pp_tile ?(show_instance = false) t =
  let pp_tile_descr = function
    | Serie (Dot, i) -> Printf.sprintf "d%i" i
    | Serie (Bam, i) -> Printf.sprintf "b%i" i
    | Serie (Char, i) -> Printf.sprintf "c%i" i
    | Dragon White -> "wd"
    | Dragon Green -> "gd"
    | Dragon Red -> "rd"
    | Wind East -> "ew"
    | Wind North -> "nw"
    | Wind West -> "ww"
    | Wind South -> "sw"
  in
  let descr, instance = descr_of_tile t in
    if show_instance then
      Printf.sprintf "%s%i" (pp_tile_descr descr) instance
    else
      pp_tile_descr descr

let d10 = 0
let d11 = 1
let d12 = 2
let d13 = 3
let d20 = 4
let d21 = 5
let d22 = 6
let d23 = 7
let d30 = 8
let d31 = 9
let d32 = 10
let d33 = 11
let d40 = 12
let d41 = 13
let d42 = 14
let d43 = 15
let d50 = 16
let d51 = 17
let d52 = 18
let d53 = 19
let d60 = 20
let d61 = 21
let d62 = 22
let d63 = 23
let d70 = 24
let d71 = 25
let d72 = 26
let d73 = 27
let d80 = 28
let d81 = 29
let d82 = 30
let d83 = 31
let d90 = 32
let d91 = 33
let d92 = 34
let d93 = 35
let b10 = 36
let b11 = 37
let b12 = 38
let b13 = 39
let b20 = 40
let b21 = 41
let b22 = 42
let b23 = 43
let b30 = 44
let b31 = 45
let b32 = 46
let b33 = 47
let b40 = 48
let b41 = 49
let b42 = 50
let b43 = 51
let b50 = 52
let b51 = 53
let b52 = 54
let b53 = 55
let b60 = 56
let b61 = 57
let b62 = 58
let b63 = 59
let b70 = 60
let b71 = 61
let b72 = 62
let b73 = 63
let b80 = 64
let b81 = 65
let b82 = 66
let b83 = 67
let b90 = 68
let b91 = 69
let b92 = 70
let b93 = 71
let c10 = 72
let c11 = 73
let c12 = 74
let c13 = 75
let c20 = 76
let c21 = 77
let c22 = 78
let c23 = 79
let c30 = 80
let c31 = 81
let c32 = 82
let c33 = 83
let c40 = 84
let c41 = 85
let c42 = 86
let c43 = 87
let c50 = 88
let c51 = 89
let c52 = 90
let c53 = 91
let c60 = 92
let c61 = 93
let c62 = 94
let c63 = 95
let c70 = 96
let c71 = 97
let c72 = 98
let c73 = 99
let c80 = 100
let c81 = 101
let c82 = 102
let c83 = 103
let c90 = 104
let c91 = 105
let c92 = 106
let c93 = 107
let wd0 = 108
let wd1 = 109
let wd2 = 110
let wd3 = 111
let gd0 = 112
let gd1 = 113
let gd2 = 114
let gd3 = 115
let rd0 = 116
let rd1 = 117
let rd2 = 118
let rd3 = 119
let ew0 = 120
let ew1 = 121
let ew2 = 122
let ew3 = 123
let sw0 = 124
let sw1 = 125
let sw2 = 126
let sw3 = 127
let ww0 = 128
let ww1 = 129
let ww2 = 130
let ww3 = 131
let nw0 = 132
let nw1 = 133
let nw2 = 134
let nw3 = 135
