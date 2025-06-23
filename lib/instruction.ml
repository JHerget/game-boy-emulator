type target =
        | A | B | C | D | E | F | H | L | BC | DE | HL | PC | SP

let string_of_target = function
    | A -> "A"
    | B -> "B"
    | C -> "C"
    | D -> "D"
    | E -> "E"
    | F -> "F"
    | H -> "H"
    | L -> "L"
    | BC -> "BC"
    | DE -> "DE"
    | HL -> "HL"
    | PC -> "PC"
    | SP -> "SP"

type t =
    | LoadByte of target
    | Add of target

let from_byte = function
    | 0x06 -> LoadByte(B)
    | 0x0E -> LoadByte(C)
    | 0x16 -> LoadByte(D)
    | 0x1E -> LoadByte(E)
    | 0x26 -> LoadByte(H)
    | 0x2E -> LoadByte(L)
    | opcode -> failwith ("Unsupported opcode: " ^ string_of_int opcode)
