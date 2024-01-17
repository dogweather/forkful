---
title:                "Att arbeta med csv"
html_title:           "Rust: Att arbeta med csv"
simple_title:         "Att arbeta med csv"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Arbetet med Comma Separated Values (CSV) är en vanlig uppgift för programmerare. CSV-filer fungerar som en strukturerad och läsbar metod för att lagra och överföra stora mängder data, särskilt tabeller eller listor med information. Därför är det viktigt för programmerare att kunna arbeta med CSV-filer för att hantera data effektivt.

## Hur man gör:
Här är ett exempel på hur man kan arbeta med CSV i Rust:

```Rust
use std::fs::File;
use csv::Reader;
fn main() {
    let file = File::open("exempel.csv").unwrap();
    let mut reader = csv::Reader::from_reader(file);
    for result in reader.records() {
        let record = result.unwrap();
        println!("{} - {}", record[0], record[1]);
    }
}
```

Exempelutdatat skulle se ut så här:

```
1 - Förnamn
2 - Efternamn
3 - Ålder
4 - Yrke
```

## Djupdykning:
CSV-filer uppfanns på 1970-talet och har sedan dess varit en populär metod för att lagra och överföra data. Alternativ till CSV inkluderar XML och JSON men CSV är fortfarande populärt på grund av sin enkelhet och läsbarhet. I Rust finns det olika paket, som "csv" som används i exemplet ovan, som gör det enkelt att arbeta med CSV-filer. Det är viktigt att uppmärksamma eventuella uppdateringar till dessa paket för att se till att din kod fortsätter att fungera korrekt.

## Se även:
För mer information om att arbeta med CSV i Rust, se följande källor:

- [csv paketets dokumentation] (https://docs.rs/csv)
- [Rust Standardbibliotekets dokumentation om filer och I/O] (https://doc.rust-lang.org/std/fs/index.html)

Lycka till med ditt arbete med CSV-filer i Rust!