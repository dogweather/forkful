---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Kommandoradsargument är data som skickas till ett program när det startas. Programmerare använder dem för att anpassa programmets utförande.

## Hur man gör:

Här är ett enkelt exempel på hur man läser in kommandoradsargument i Rust:

```Rust
fn main() {
    for argument in std::env::args() {
        println!("{}", argument);
    }
}
```

Kör programmet med `cargo run arg1 arg2 arg3` ger följande utskrift:

```
/path/to/your/program
arg1
arg2
arg3
```

## Djupdykning

Historiskt sett har kommandoradsargument använts sedan tidiga textbaserade operativsystem som UNIX. Alternativ till `std::env::args()` inkluderar `std::env::args_os()` som returnerar OsString-objekt istället för String-objekt, vilket kan vara användbart när du behöver stödja data som inte nödvändigtvis kan representeras som giltig UTF-8.

En viktig detalj är att `std::env::args()` inkluderar programmets sökväg som det första argumentet, som vi såg i exemplet ovan. Om du bara vill ha argument som skickades till programmet, och inte sökvägen till programmet, kan du använda `std::env::args().skip(1)`.

## Se även 

För mer information om Rusts kommandorads gränssnitt, se följande länkar:

- [std::env i Rust-Dokumentation](https://doc.rust-lang.org/std/env/index.html)
- [Command Line Argumenter i Rust-Lehre](https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-command-line-macros.html)