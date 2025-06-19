type t = {
    entry: bytes;
    nintendo_logo: bytes;
    title: string;
    manufacturer_code: string;
    cgb_flag: int;
    new_licensee_code: string;
    sgb_flag: int;
    cartridge_type: int;
    rom_size: int;
    ram_size: int;
    destination_code: int;
    old_licensee_code: int;
    rom_version_number: int;
    header_checksum: int;
    global_checksum: bytes;
    data: bytes;
}

let new_licensee = function
    | "01" -> "Nintendo Research & Development 1"
    | "08" -> "Capcom"
    | "13" -> "EA (Electronic Arts)"
    | "18" -> "Hudson Soft"
    | "19" -> "B-AI"
    | "20" -> "KSS"
    | "22" -> "Planning Office WADA"
    | "24" -> "PCM Complete"
    | "25" -> "San-X"
    | "28" -> "Kemco"
    | "29" -> "SETA Corporation"
    | "30" -> "Viacom"
    | "31" -> "Nintendo"
    | "32" -> "Bandai"
    | "33" -> "Ocean Software/Acclaim Entertainment"
    | "34" -> "Konami"
    | "35" -> "HectorSoft"
    | "37" -> "Taito"
    | "38" -> "Hudson Soft"
    | "39" -> "Banpresto"
    | "41" -> "Ubi Soft1"
    | "42" -> "Atlus"
    | "44" -> "Malibu Interactive"
    | "46" -> "Angel"
    | "47" -> "Bullet-Proof Software"
    | "49" -> "Irem"
    | "50" -> "Absolute"
    | "51" -> "Acclaim Entertainment"
    | "52" -> "Activision"
    | "53" -> "Sammy USA Corporation"
    | "54" -> "Konami"
    | "55" -> "Hi Tech Expressions"
    | "56" -> "LJN"
    | "57" -> "Matchbox"
    | "58" -> "Mattel"
    | "59" -> "Milton Bradley Company"
    | "60" -> "Titus Interactive"
    | "61" -> "Virgin Games Ltd."
    | "64" -> "Lucasfilm Games"
    | "67" -> "Ocean Software"
    | "69" -> "EA (Electronic Arts)"
    | "70" -> "Infogrames"
    | "71" -> "Interplay Entertainment"
    | "72" -> "Broderbund"
    | "73" -> "Sculptured Software"
    | "75" -> "The Sales Curve Limited"
    | "78" -> "THQ"
    | "79" -> "Accolade"
    | "80" -> "Misawa Entertainment"
    | "83" -> "lozc"
    | "86" -> "Tokuma Shoten"
    | "87" -> "Tsukuda Original"
    | "91" -> "Chunsoft Co."
    | "92" -> "Video System"
    | "93" -> "Ocean Software/Acclaim Entertainment"
    | "95" -> "Varie"
    | "96" -> "Yonezawa/s’pal"
    | "97" -> "Kaneko"
    | "99" -> "Pack-In-Video"
    | "9H" -> "Bottom Up"
    | "A4" -> "Konami (Yu-Gi-Oh!)"
    | "BL" -> "MTO"
    | "DK" -> "Kodansha"
    | _ -> "None"
;;

let old_licensee = function
    | 0x01 -> "Nintendo"
    | 0x08 -> "Capcom"
    | 0x09 -> "HOT-B"
    | 0x0A -> "Jaleco"
    | 0x0B -> "Coconuts Japan"
    | 0x0C -> "Elite Systems"
    | 0x13 -> "EA (Electronic Arts)"
    | 0x18 -> "Hudson Soft"
    | 0x19 -> "ITC Entertainment"
    | 0x1A -> "Yanoman"
    | 0x1D -> "Japan Clary"
    | 0x1F -> "Virgin Games Ltd."
    | 0x24 -> "PCM Complete"
    | 0x25 -> "San-X"
    | 0x28 -> "Kemco"
    | 0x29 -> "SETA Corporation"
    | 0x30 -> "Infogrames"
    | 0x31 -> "Nintendo"
    | 0x32 -> "Bandai"
    | 0x33 -> "Use new licensee"
    | 0x34 -> "Konami"
    | 0x35 -> "HectorSoft"
    | 0x38 -> "Capcom"
    | 0x39 -> "Banpresto"
    | 0x3C -> "Entertainment Interactive (stub)"
    | 0x3E -> "Gremlin"
    | 0x41 -> "Ubi Soft"
    | 0x42 -> "Atlus"
    | 0x44 -> "Malibu Interactive"
    | 0x46 -> "Angel"
    | 0x47 -> "Spectrum HoloByte"
    | 0x49 -> "Irem"
    | 0x4A -> "Virgin Games Ltd."
    | 0x4D -> "Malibu Interactive"
    | 0x4F -> "U.S. Gold"
    | 0x50 -> "Absolute"
    | 0x51 -> "Acclaim Entertainment"
    | 0x52 -> "Activision"
    | 0x53 -> "Sammy USA Corporation"
    | 0x54 -> "GameTek"
    | 0x55 -> "Park Place"
    | 0x56 -> "LJN"
    | 0x57 -> "Matchbox"
    | 0x59 -> "Milton Bradley Company"
    | 0x5A -> "Mindscape"
    | 0x5B -> "Romstar"
    | 0x5C -> "Naxat Soft14"
    | 0x5D -> "Tradewest"
    | 0x60 -> "Titus Interactive"
    | 0x61 -> "Virgin Games Ltd."
    | 0x67 -> "Ocean Software"
    | 0x69 -> "EA (Electronic Arts)"
    | 0x6E -> "Elite Systems"
    | 0x6F -> "Electro Brain"
    | 0x70 -> "Infogrames5"
    | 0x71 -> "Interplay Entertainment"
    | 0x72 -> "Broderbund"
    | 0x73 -> "Sculptured Software"
    | 0x75 -> "The Sales Curve Limited"
    | 0x78 -> "THQ"
    | 0x79 -> "Accolade"
    | 0x7A -> "Triffix Entertainment"
    | 0x7C -> "MicroProse"
    | 0x7F -> "Kemco"
    | 0x80 -> "Misawa Entertainment"
    | 0x83 -> "LOZC G."
    | 0x86 -> "Tokuma Shoten"
    | 0x8B -> "Bullet-Proof Software"
    | 0x8C -> "Vic Tokai Corp."
    | 0x8E -> "Ape Inc."
    | 0x8F -> "I’Max18"
    | 0x91 -> "Chunsoft Co."
    | 0x92 -> "Video System"
    | 0x93 -> "Tsubaraya Productions"
    | 0x95 -> "Varie"
    | 0x96 -> "Yonezawa/S’Pal"
    | 0x97 -> "Kemco"
    | 0x99 -> "Arc"
    | 0x9A -> "Nihon Bussan"
    | 0x9B -> "Tecmo"
    | 0x9C -> "Imagineer"
    | 0x9D -> "Banpresto"
    | 0x9F -> "Nova"
    | 0xA1 -> "Hori Electric"
    | 0xA2 -> "Bandai"
    | 0xA4 -> "Konami"
    | 0xA6 -> "Kawada"
    | 0xA7 -> "Takara"
    | 0xA9 -> "Technos Japan"
    | 0xAA -> "Broderbund"
    | 0xAC -> "Toei Animation"
    | 0xAD -> "Toho"
    | 0xAF -> "Namco"
    | 0xB0 -> "Acclaim Entertainment"
    | 0xB1 -> "ASCII Corporation or Nexsoft"
    | 0xB2 -> "Bandai"
    | 0xB4 -> "Square Enix"
    | 0xB6 -> "HAL Laboratory"
    | 0xB7 -> "SNK"
    | 0xB9 -> "Pony Canyon"
    | 0xBA -> "Culture Brain"
    | 0xBB -> "Sunsoft"
    | 0xBD -> "Sony Imagesoft"
    | 0xBF -> "Sammy Corporation"
    | 0xC0 -> "Taito"
    | 0xC2 -> "Kemco"
    | 0xC3 -> "Square"
    | 0xC4 -> "Tokuma Shoten"
    | 0xC5 -> "Data East"
    | 0xC6 -> "Tonkin House"
    | 0xC8 -> "Koei"
    | 0xC9 -> "UFL"
    | 0xCA -> "Ultra Games"
    | 0xCB -> "VAP, Inc."
    | 0xCC -> "Use Corporation"
    | 0xCD -> "Meldac"
    | 0xCE -> "Pony Canyon"
    | 0xCF -> "Angel"
    | 0xD0 -> "Taito"
    | 0xD1 -> "SOFEL (Software Engineering Lab)"
    | 0xD2 -> "Quest"
    | 0xD3 -> "Sigma Enterprises"
    | 0xD4 -> "ASK Kodansha Co."
    | 0xD6 -> "Naxat Soft"
    | 0xD7 -> "Copya System"
    | 0xD9 -> "Banpresto"
    | 0xDA -> "Tomy"
    | 0xDB -> "LJN"
    | 0xDD -> "Nippon Computer Systems"
    | 0xDE -> "Human Ent."
    | 0xDF -> "Altron"
    | 0xE0 -> "Jaleco"
    | 0xE1 -> "Towa Chiki"
    | 0xE2 -> "Yutaka"
    | 0xE3 -> "Varie"
    | 0xE5 -> "Epoch"
    | 0xE7 -> "Athena"
    | 0xE8 -> "Asmik Ace Entertainment"
    | 0xE9 -> "Natsume"
    | 0xEA -> "King Records"
    | 0xEB -> "Atlus"
    | 0xEC -> "Epic/Sony Records"
    | 0xEE -> "IGS"
    | 0xF0 -> "A Wave"
    | 0xF3 -> "Extreme Entertainment"
    | 0xFF -> "LJN"
    | _ -> "Unknown"
;;

let cartridge_type = function
    | 0x00 -> "ROM ONLY"
    | 0x01 -> "MBC1"
    | 0x02 -> "MBC1+RAM"
    | 0x03 -> "MBC1+RAM+BATTERY"
    | 0x05 -> "MBC2"
    | 0x06 -> "MBC2+BATTERY"
    | 0x08 -> "ROM+RAM"
    | 0x09 -> "ROM+RAM+BATTERY"
    | 0x0B -> "MMM01"
    | 0x0C -> "MMM01+RAM"
    | 0x0D -> "MMM01+RAM+BATTERY"
    | 0x0F -> "MBC3+TIMER+BATTERY"
    | 0x10 -> "MBC3+TIMER+RAM+BATTERY"
    | 0x11 -> "MBC3"
    | 0x12 -> "MBC3+RAM"
    | 0x13 -> "MBC3+RAM+BATTERY"
    | 0x19 -> "MBC5"
    | 0x1A -> "MBC5+RAM"
    | 0x1B -> "MBC5+RAM+BATTERY"
    | 0x1C -> "MBC5+RUMBLE"
    | 0x1D -> "MBC5+RUMBLE+RAM"
    | 0x1E -> "MBC5+RUMBLE+RAM+BATTERY"
    | 0x20 -> "MBC6"
    | 0x22 -> "MBC7+SENSOR+RUMBLE+RAM+BATTERY"
    | 0xFC -> "POCKET CAMERA"
    | 0xFD -> "BANDAI TAMA5"
    | 0xFE -> "HuC3"
    | 0xFF -> "HuC1+RAM+BATTERY"
    | _ -> "Unknown"
;;

let rom_size = function
    | 0x00 -> "32 KiB"
    | 0x01 -> "64 KiB"
    | 0x02 -> "128 KiB"
    | 0x03 -> "256 KiB"
    | 0x04 -> "512 KiB"
    | 0x05 -> "1 MiB"
    | 0x06 -> "2 MiB"
    | 0x07 -> "4 MiB"
    | 0x08 -> "8 MiB"
    | 0x52 -> "1.1 MiB"
    | 0x53 -> "1.2 MiB"
    | 0x54 -> "1.5 MiB"
    | _ -> "Unknown"
;;

let ram_size = function
    | 0x01 -> "Unused"
    | 0x02 -> "8 KiB (1 bank)"
    | 0x03 -> "32 KiB (4 banks of 8 KiB each)"
    | 0x04 -> "128 KiB (16 banks of 8 KiB each)"
    | 0x05 -> "64 KiB (8 banks of 8 KiB each)"
    | _ -> "0 (No RAM)"
;;

let destination = function
    | 0x00 -> "Japan (and possibly overseas)"
    | 0x01 -> "Overseas only"
    | _ -> "Unknown"
;;

let load file_path =
    let rom_bytes = In_channel.with_open_bin file_path
        @@ fun ic ->
            let contents = In_channel.input_all ic in
            Bytes.of_string contents
    in
    let slice start stop =
        Bytes.sub rom_bytes start (stop + 1 - start)
    in
    let get index =
        int_of_char @@ Bytes.get rom_bytes index
    in
    {
        entry = slice 0x0100 0x0103;
        nintendo_logo = slice 0x0104 0x0133;
        title = Bytes.to_string @@ slice 0x0134 0x0143;
        manufacturer_code = Bytes.to_string @@ slice 0x013F 0x0142;
        cgb_flag = get 0x0143;
        new_licensee_code = Bytes.to_string @@ slice 0x0144 0x0145;
        sgb_flag = get 0x0146;
        cartridge_type = get 0x0147;
        rom_size = get 0x0148;
        ram_size = get 0x0149;
        destination_code = get 0x014A;
        old_licensee_code = get 0x014B;
        rom_version_number = get 0x014C;
        header_checksum = get 0x014D;
        global_checksum = slice 0x014E 0x014F;
        data = rom_bytes;
    }
;;

let print_header rom =
    Printf.printf "Title: %s\n" rom.title;
    if rom.old_licensee_code = 51 then
        Printf.printf "Licensee: %s\n" @@ new_licensee rom.new_licensee_code
    else
        Printf.printf "Licensee: %s\n" @@ old_licensee rom.old_licensee_code;
    Printf.printf "Cartridge type: %s\n" @@ cartridge_type rom.cartridge_type;
    Printf.printf "ROM size: %s\n" @@ rom_size rom.rom_size;
    Printf.printf "RAM size: %s\n" @@ ram_size rom.ram_size;
    Printf.printf "ROM version: %d\n" rom.rom_version_number;
    Printf.printf "Destination: %s\n" @@ destination rom.destination_code
;;
