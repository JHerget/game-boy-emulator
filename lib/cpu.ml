module MemoryBus = struct
    type t = {
        memory: int array;
    }

    let read_byte bus address =
        bus.memory.(address)
    ;;   

    let write_byte bus address value =
        bus.memory.(address) <- value
    ;;   
end

type t = {
    registers: Registers.t;
    bus: MemoryBus.t;
    mutable pc: int;
    mutable sp: int;
}

let init () =
    {
        registers = {
            a = 0;
            b = 0;
            c = 0;
            d = 0;
            e = 0;
            f = {
                zero = false;
                subtract = false;
                half_carry = false;
                carry = false;
            };
            h = 0;
            l = 0;
        };
        bus = { memory = Array.make 0xFFFF 0 };
        pc = 0;
        sp = 0;
    }
;;

let add cpu value =
    let bit_mask = 0xFF in
    let sum = value + cpu.registers.a in
    let did_overflow = sum > bit_mask in
    let new_value = sum land bit_mask in

    cpu.registers.f.zero <- new_value = 0;
    cpu.registers.f.subtract <- false;
    cpu.registers.f.carry <- did_overflow;
    cpu.registers.f.half_carry <-
        (value land 0xF) + (cpu.registers.a land 0xF) > 0xF;

    new_value
;;

let execute cpu = function
    | Instructions.LoadByte(target) ->
        let value = MemoryBus.read_byte cpu.bus cpu.pc in
        let mask = 0xFF in

        cpu.pc <- cpu.pc + 1;

        (match target with
        | B -> cpu.registers.b <- (value land mask)
        | C -> cpu.registers.c <- (value land mask)
        | D -> cpu.registers.d <- (value land mask)
        | E -> cpu.registers.e <- (value land mask)
        | H -> cpu.registers.h <- (value land mask)
        | L -> cpu.registers.l <- (value land mask)
        | HL ->
            let address = Registers.get_hl cpu.registers in
            MemoryBus.write_byte cpu.bus address value
        | _ -> failwith ("Unsupported target for LoadByte instruction: " ^ Instructions.string_of_target target))
    | Instructions.LoadRegister(target, source) ->
        (match (target, source) with
        | (A, A) -> cpu.registers.a <- cpu.registers.a
        | (A, B) -> cpu.registers.a <- cpu.registers.b
        | (A, C) -> cpu.registers.a <- cpu.registers.c
        | (A, D) -> cpu.registers.a <- cpu.registers.d
        | (A, E) -> cpu.registers.a <- cpu.registers.e
        | (A, H) -> cpu.registers.a <- cpu.registers.h
        | (A, L) -> cpu.registers.a <- cpu.registers.l
        | (B, B) -> cpu.registers.b <- cpu.registers.b
        | (B, C) -> cpu.registers.b <- cpu.registers.c
        | (B, D) -> cpu.registers.b <- cpu.registers.d
        | (B, E) -> cpu.registers.b <- cpu.registers.e
        | (B, H) -> cpu.registers.b <- cpu.registers.h
        | (B, L) -> cpu.registers.b <- cpu.registers.l
        | (C, B) -> cpu.registers.c <- cpu.registers.b
        | (C, C) -> cpu.registers.c <- cpu.registers.c
        | (C, D) -> cpu.registers.c <- cpu.registers.d
        | (C, E) -> cpu.registers.c <- cpu.registers.e
        | (C, H) -> cpu.registers.c <- cpu.registers.h
        | (C, L) -> cpu.registers.c <- cpu.registers.l
        | (D, B) -> cpu.registers.d <- cpu.registers.b
        | (D, C) -> cpu.registers.d <- cpu.registers.c
        | (D, D) -> cpu.registers.d <- cpu.registers.d
        | (D, E) -> cpu.registers.d <- cpu.registers.e
        | (D, H) -> cpu.registers.d <- cpu.registers.h
        | (D, L) -> cpu.registers.d <- cpu.registers.l
        | (E, B) -> cpu.registers.e <- cpu.registers.b
        | (E, C) -> cpu.registers.e <- cpu.registers.c
        | (E, D) -> cpu.registers.e <- cpu.registers.d
        | (E, E) -> cpu.registers.e <- cpu.registers.e
        | (E, H) -> cpu.registers.e <- cpu.registers.h
        | (E, L) -> cpu.registers.e <- cpu.registers.l
        | (H, B) -> cpu.registers.h <- cpu.registers.b
        | (H, C) -> cpu.registers.h <- cpu.registers.c
        | (H, D) -> cpu.registers.h <- cpu.registers.d
        | (H, E) -> cpu.registers.h <- cpu.registers.e
        | (H, H) -> cpu.registers.h <- cpu.registers.h
        | (H, L) -> cpu.registers.h <- cpu.registers.l
        | (L, B) -> cpu.registers.l <- cpu.registers.b
        | (L, C) -> cpu.registers.l <- cpu.registers.c
        | (L, D) -> cpu.registers.l <- cpu.registers.d
        | (L, E) -> cpu.registers.l <- cpu.registers.e
        | (L, H) -> cpu.registers.l <- cpu.registers.h
        | (L, L) -> cpu.registers.l <- cpu.registers.l
        | (_, HL) ->
            let address = Registers.get_hl cpu.registers in
            let value = MemoryBus.read_byte cpu.bus address in
            (match target with
            | A -> cpu.registers.a <- value
            | B -> cpu.registers.b <- value
            | C -> cpu.registers.c <- value
            | D -> cpu.registers.d <- value
            | E -> cpu.registers.e <- value
            | H -> cpu.registers.h <- value
            | L -> cpu.registers.l <- value
            | _ -> failwith ("Unsupported target for source (HL) for the LoadRegister instruction: " ^ Instructions.string_of_target target))
        | (HL, _) ->
            let address = Registers.get_hl cpu.registers in
            let write value =
                MemoryBus.write_byte cpu.bus address value
            in
            (match source with
            | B -> write cpu.registers.b
            | C -> write cpu.registers.c
            | D -> write cpu.registers.d
            | E -> write cpu.registers.e
            | H -> write cpu.registers.h
            | L -> write cpu.registers.l
            | _ -> failwith ("Unsupported source for target (HL) for the LoadRegister instruction: " ^ Instructions.string_of_target source))
        | (_, _) -> failwith ("Unsupported target or source for LoadRegister instruction: " ^ Instructions.string_of_target target ^ " " ^ Instructions.string_of_target source))
    | Instructions.Add(target) ->
        (match target with
        | C ->
            let value = cpu.registers.c in
            let new_value = add cpu value in
            cpu.registers.a <- new_value
        | _ -> failwith ("Unsupported target for Add instruction: " ^ Instructions.string_of_target target))
;;
