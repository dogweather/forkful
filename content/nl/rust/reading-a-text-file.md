---
title:                "Een tekstbestand lezen"
date:                  2024-01-28T22:05:01.404213-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tekstbestand lezen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/rust/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een tekstbestand lezen is het verkrijgen van tekstinhoud uit een .txt-bestand op je schijf. Programmeurs doen dit om gegevens te verwerken zoals configuratie, gebruikersinvoer of om bulktekst te verwerken.

## Hoe:
De standaardbibliotheek van Rust maakt het eenvoudig om bestanden te lezen.

```Rust
use std::fs::File;
use std::io::{self, Read};

fn main() -> io::Result<()> {
    let mut file = File::open("example.txt")?;
    let mut inhoud = String::new();
    file.read_to_string(&mut inhoud)?;
    println!("Bestandsinhoud:\n{}", inhoud);
    Ok(())
}
```
Deze code opent "example.txt", leest het en drukt de inhoud af.

Voorbeelduitvoer:
```
Bestandsinhoud:
Hallo, Rustaceans!
```

## Diepere Duik
Historisch gezien kan bestands-I/O complex zijn, maar Rust vereenvoudigt dit. Er zijn alternatieven voor `read_to_string`, zoals het gebruik van `BufRead` voor regel-voor-regel afhandeling, wat efficiënter is voor grotere bestanden. Intern maakt Rust's bestandslezing gebruik van systeemaanroepen op OS-niveau, en buffer data voor efficiëntie.

Na Rust 1.0 benadrukt de taal veilige systeeminteracties – het lezen van een bestand is geen uitzondering. Het `Result` type omvat potentiële fouten, waardoor Rust robuust is tegen veelvoorkomende valkuilen zoals ontbrekende bestanden of toestemmingsproblemen zonder te hoeven terugvallen op paniek.

## Zie Ook
Aanvullende bronnen om te bekijken:
- Rust's documentatie over bestands-I/O: [std::fs](https://doc.rust-lang.org/std/fs/)
- Het hoofdstuk over foutafhandeling in Het Boek: [Foutafhandeling](https://doc.rust-lang.org/book/ch09-00-error-handling.html)
- Rust bij Voorbeeld over bestands-I/O: [Bestands-I/O](https://doc.rust-lang.org/rust-by-example/std_misc/file.html)