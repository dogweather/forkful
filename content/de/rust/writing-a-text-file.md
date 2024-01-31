---
title:                "Eine Textdatei schreiben"
date:                  2024-01-19
html_title:           "Arduino: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"

category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Schreiben in eine Textdatei bedeutet, Daten in lesbarem Format zu speichern. Programmierer machen das, um Daten dauerhaft zu sichern und für Menschen und Programme leicht zugänglich zu machen.

## How to:
```Rust
use std::fs::File;
use std::io::Write;

fn main() -> std::io::Result<()> {
    let mut file = File::create("beispiel.txt")?;
    file.write_all(b"Hallo, Rustaceans!")?;
    Ok(())
}
```
Output in `beispiel.txt`:
```
Hallo, Rustaceans!
```

## Deep Dive
Textdateien schreiben ist seit den Anfängen von Unix ein grundlegender Bestandteil der Programmierung. Alternativen umfassen Datenbanken oder Binärdateien, die Struktur und Performance verbessern können. In Rust nutzt man `std::fs` und `std::io` für Dateioperationen. Dank Error-Handling über das `Result`-Typ sind Operationen sicher und fehlertolerant.

## See Also
- [The Rust Programming Language – File I/O](https://doc.rust-lang.org/book/ch12-02-reading-a-file.html)
- [Rust by Example – File I/O](https://doc.rust-lang.org/rust-by-example/std_misc/file.html)
- [Rust std::fs Module](https://doc.rust-lang.org/std/fs/index.html)
