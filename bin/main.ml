open Emulator

let () =
    let rom = Rom.load "./roms/legend-of-zelda-links-awakening-gb" in
    print_endline "";
    Rom.print_header rom
