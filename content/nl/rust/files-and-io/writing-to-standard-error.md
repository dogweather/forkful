---
title:                "Schrijven naar standaardfout"
aliases: - /nl/rust/writing-to-standard-error.md
date:                  2024-01-28T22:13:55.510883-07:00
model:                 gpt-4-0125-preview
simple_title:         "Schrijven naar standaardfout"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/rust/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Schrijven naar standaardfout (stderr) betekent tekst uitvoeren naar de foutenstroom, los van de standaarduitvoer (stdout). Programmeurs doen dit om fouten en diagnostische berichten te loggen zonder de reguliere uitvoer te vervuilen, die mogelijk wordt omgeleid of doorgegeven aan andere programma's.

## Hoe:

Rust maakt het schrijven naar stderr eenvoudig. Gebruik de macro `eprintln!` voor tekst, net zoals `println!`, maar dan voor fouten.

```Rust
fn main() {
    // Reguliere uitvoer
    println!("Dit is een regulier bericht.");

    // Foutuitvoer
    eprintln!("Dit is een foutbericht.");
}
```

Voorbeelduitvoer:

```shell
Dit is een regulier bericht.
Dit is een foutbericht.
```

Merk op dat het foutbericht naar stderr gaat. In een terminal zul je het verschil niet zien. Echter, als je stdout omleidt, verschijnt stderr nog steeds in de console.

```shell
$ cargo run > output.txt
Dit is een foutbericht.
```

Hier zal `output.txt` alleen "Dit is een regulier bericht." bevatten.

## Diepgaande Verkenning

Het scheiden van stdout en stderr stelt Unix-systemen historisch in staat om reguliere en foutgegevens anders te behandelen. Het is een goede praktijk en helpt bij automatisering en logging.

Alternatieven voor het schrijven naar stderr zijn lager niveau, zoals het gebruik van `std::io::stderr`. Dit geeft meer controle en werkt goed voor niet-tekstgegevens.

```Rust
use std::io::{self, Write};

fn main() -> io::Result<()> {
    let stderr = &mut io::stderr();
    
    // Schrijf een string rechtstreeks naar stderr
    writeln!(stderr, "Fout: De operatie kon niet voltooid worden")?;
    
    Ok(())
}
```

Binnen de motorkap, `eprintln!` is een macro die `writeln!` naar stderr omwikkelt, waardoor dingen DRY blijven (Don't Repeat Yourself - Herhaal Jezelf Niet).

## Zie Ook

Voor meer over foutafhandeling en logging:

- Rust Bij Voorbeeld over stdio: https://doc.rust-lang.org/rust-by-example/std_misc/stdio.html
- Het Rust Boek over Foutafhandeling: https://doc.rust-lang.org/book/ch09-00-error-handling.html
- De Rust `log` crate voor een meer uitgebreide logopstelling: https://crates.io/crates/log
