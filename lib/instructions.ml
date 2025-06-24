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
    | LoadRegister of target * target
    | Add of target

let from_byte = function
    | 0x06 -> LoadByte(B)
    | 0x0E -> LoadByte(C)
    | 0x16 -> LoadByte(D)
    | 0x1E -> LoadByte(E)
    | 0x26 -> LoadByte(H)
    | 0x2E -> LoadByte(L)
    | 0x36 -> LoadByte(HL)
    | 0x7F -> LoadRegister(A, A)
    | 0x78 -> LoadRegister(A, B)
    | 0x79 -> LoadRegister(A, C)
    | 0x7A -> LoadRegister(A, D)
    | 0x7B -> LoadRegister(A, E)
    | 0x7C -> LoadRegister(A, H)
    | 0x7D -> LoadRegister(A, L)
    | 0x7E -> LoadRegister(A, HL)
    | 0x40 -> LoadRegister(B, B)
    | 0x41 -> LoadRegister(B, C)
    | 0x42 -> LoadRegister(B, D)
    | 0x43 -> LoadRegister(B, E)
    | 0x44 -> LoadRegister(B, H)
    | 0x45 -> LoadRegister(B, L)
    | 0x46 -> LoadRegister(B, HL)
    | 0x48 -> LoadRegister(C, B)
    | 0x49 -> LoadRegister(C, C)
    | 0x4A -> LoadRegister(C, D)
    | 0x4B -> LoadRegister(C, E)
    | 0x4C -> LoadRegister(C, H)
    | 0x4D -> LoadRegister(C, L)
    | 0x4E -> LoadRegister(C, HL)
    | 0x50 -> LoadRegister(D, B)
    | 0x51 -> LoadRegister(D, C)
    | 0x52 -> LoadRegister(D, D)
    | 0x53 -> LoadRegister(D, E)
    | 0x54 -> LoadRegister(D, H)
    | 0x55 -> LoadRegister(D, L)
    | 0x56 -> LoadRegister(D, HL)
    | 0x58 -> LoadRegister(E, B)
    | 0x59 -> LoadRegister(E, C)
    | 0x5A -> LoadRegister(E, D)
    | 0x5B -> LoadRegister(E, E)
    | 0x5C -> LoadRegister(E, H)
    | 0x5D -> LoadRegister(E, L)
    | 0x5E -> LoadRegister(E, HL)
    | 0x60 -> LoadRegister(H, B)
    | 0x61 -> LoadRegister(H, C)
    | 0x62 -> LoadRegister(H, D)
    | 0x63 -> LoadRegister(H, E)
    | 0x64 -> LoadRegister(H, H)
    | 0x65 -> LoadRegister(H, L)
    | 0x66 -> LoadRegister(H, HL)
    | 0x68 -> LoadRegister(L, B)
    | 0x69 -> LoadRegister(L, C)
    | 0x6A -> LoadRegister(L, D)
    | 0x6B -> LoadRegister(L, E)
    | 0x6C -> LoadRegister(L, H)
    | 0x6D -> LoadRegister(L, L)
    | 0x6E -> LoadRegister(L, HL)
    | 0x70 -> LoadRegister(HL, B)
    | 0x71 -> LoadRegister(HL, C)
    | 0x72 -> LoadRegister(HL, D)
    | 0x73 -> LoadRegister(HL, E)
    | 0x74 -> LoadRegister(HL, H)
    | 0x75 -> LoadRegister(HL, L)
    | opcode -> failwith ("Unsupported opcode: " ^ string_of_int opcode)
