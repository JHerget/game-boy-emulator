open Emulator

let cpu = Cpu.init ()

let () =
    let rom = Rom.load "./roms/legend-of-zelda-links-awakening-gb" in
    print_endline "";
    Rom.print_header rom

let () =
    print_endline "";
    cpu.bus.memory <- [| 0x06; 0x2C; 0x1E; 0xA7 |];
    
    let rec run () =
        match cpu.pc with
        | pc when pc < Array.length cpu.bus.memory ->
            let opcode = Cpu.MemoryBus.read_byte cpu.bus cpu.pc in
            cpu.pc <- cpu.pc + 1;

            Cpu.execute cpu @@ Instruction.from_byte opcode;
            run ()
        | _ ->
            Printf.printf "PC: %d\n" cpu.pc;
            Printf.printf "%s\n" @@ Registers.to_string cpu.registers
    in
    run ();
