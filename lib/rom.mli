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

val new_licensee : string -> string
val old_licensee : int -> string
val cartridge_type : int -> string
val rom_size : int -> string
val ram_size : int -> string
val destination : int -> string

val load : string -> t
val print_header : t -> unit
