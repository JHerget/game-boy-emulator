type flags = {
    mutable zero: bool;
    mutable subtract: bool;
    mutable half_carry: bool;
    mutable carry: bool;
}

let zero_flag_pos = 7
let subtract_flag_pos = 6
let half_carry_flag_pos = 5
let carry_flag_pos = 4

let flags_to_byte flags =
    let bool_to_int = function
        | true -> 1
        | false -> 0
    in
    (bool_to_int flags.zero) lsl zero_flag_pos lor
    (bool_to_int flags.subtract) lsl subtract_flag_pos lor
    (bool_to_int flags.half_carry) lsl half_carry_flag_pos lor
    (bool_to_int flags.carry) lsl carry_flag_pos
;;

let byte_to_flags byte =
    let check_bit index =
        byte land (1 lsl index) <> 0
    in
    {
        zero = check_bit zero_flag_pos;
        subtract = check_bit subtract_flag_pos;
        half_carry = check_bit half_carry_flag_pos;
        carry = check_bit carry_flag_pos;
    }
;;

type t = {
    mutable a: int;
    mutable b: int;
    mutable c: int;
    mutable d: int;
    mutable e: int;
    f: flags;
    mutable h: int;
    mutable l: int;
}

let to_string regs =
    Printf.sprintf "a: %02X\nb: %02X\nc: %02X\nd: %02X\ne: %02X\nh: %02X\nl: %02X\n"
        regs.a regs.b regs.c regs.d regs.e regs.h regs.l
;;

let get_bc regs =
    (regs.b lsl 8) lor regs.c
;;

let set_bc regs dword =
    regs.b <- (dword lsr 8) land 0xFF;
    regs.c <- dword land 0xFF
;;

let get_de regs =
    (regs.d lsl 8) lor regs.e
;;

let set_de regs dword =
    regs.d <- (dword lsr 8) land 0xFF;
    regs.e <- dword land 0xFF
;;

let get_hl regs =
    (regs.h lsl 8) lor regs.l
;;

let set_hl regs dword =
    regs.h <- (dword lsr 8) land 0xFF;
    regs.l <- dword land 0xFF
;;
