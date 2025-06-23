module MemoryBus = struct
    type t = {
        mutable memory: int list;
    }

    let read_byte bus address =
        List.nth bus.memory address
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
        bus = { memory = [] };
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
    | Instruction.LoadByte(target) ->
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
        | _ -> failwith ("Unsupported target for LoadByte instruction: " ^ Instruction.string_of_target target))
    | Instruction.Add(target) ->
        (match target with
        | C ->
            let value = cpu.registers.c in
            let new_value = add cpu value in
            cpu.registers.a <- new_value
        | _ -> failwith ("Unsupported target for Add instruction: " ^ Instruction.string_of_target target))
;;
