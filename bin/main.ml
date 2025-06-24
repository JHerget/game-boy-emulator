open Emulator

let cpu = Cpu.init ()

let () =
    let rom = Rom.load "./roms/legend-of-zelda-links-awakening-gb" in
    print_endline "";
    Rom.print_header rom

let () =
    print_endline "";
    
    Cpu.MemoryBus.write_byte cpu.bus 0x0000 0x0006;
    Cpu.MemoryBus.write_byte cpu.bus 0x0001 0x000A;
    Cpu.MemoryBus.write_byte cpu.bus 0x0002 0x0068;
    Cpu.MemoryBus.write_byte cpu.bus 0x0003 0x0075;

    print_endline "Memory:";
    Array.iter (fun x -> Printf.printf "%02X\n" x) @@ Array.sub cpu.bus.memory 0 15;
    print_endline "";

    let rec run () =
        match cpu.pc with
        | pc when Cpu.MemoryBus.read_byte cpu.bus pc <> 0x0000 ->
            let opcode = Cpu.MemoryBus.read_byte cpu.bus cpu.pc in
            cpu.pc <- cpu.pc + 1;

            Cpu.execute cpu @@ Instructions.from_byte opcode;
            run ()
        | _ ->
            Printf.printf "PC: %d\n" cpu.pc;
            Printf.printf "%s\n" @@ Registers.to_string cpu.registers
    in
    run ();

    print_endline "Memory:";
    Array.iter (fun x -> Printf.printf "%02X\n" x) @@ Array.sub cpu.bus.memory 0 15;
