---
title:    "Rust: Att få den aktuella datumet."
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Varför

Att kunna hämta den aktuella datumet i ett programspråk är en användbar funktion för många olika applikationer. Oavsett om du vill visa den aktuella datumet på en hemsida, lägga till datumet i en filnamn eller bara hålla koll på när ett program senast kördes, är det viktigt att vara bekant med hur man hämtar datumet i Rust.

## Så här gör du

Det finns flera sätt att hämta den aktuella datumet i Rust, men en vanlig metod är att använda den inbyggda standardbiblioteket chrono. För att använda detta bibliotek behöver du lägga till den i ditt projekt med hjälp av cargo.toml:

```Rust
[dependencies]
chrono = "0.4.19"
```

När du har importerat biblioteket, kan du använda det för att skapa ett datumobjekt och sedan hämta den aktuella datumet:

```Rust
use chrono::{Local, NaiveDate};

let datum = Local::now().date(); // Skapar ett datumobjekt för den aktuella lokala tiden
println!("Idag är det {}", datum); // Skriver ut den aktuella datumet i formatet "ÅÅÅÅ-MM-DD"
```

Detta kodexempel använder Local::now() för att hämta den aktuella tiden i din lokala tidszon och .date() för att få datum-delen av detta objekt. Du kan sedan använda olika funktioner för att formatera eller manipulera datumet efter behov.

## Djupdykning

Den föregående koden visar bara en grundläggande metod för att hämta den aktuella datumet i Rust, men det finns många andra sätt att göra det. Till exempel kan du använda olika tidszoner, hantera datum i annat format eller beräkna skillnaden mellan två datum. För en djupare förståelse av hur man hanterar datum i Rust, kan du läsa dokumentationen för biblioteket chrono eller utforska andra alternativ som är tillgängliga.

## Se även

- [Datum och tid i Rust dokumentationen](https://doc.rust-lang.org/std/time/index.html)
- [Chrono biblioteket på Crates.io](https://crates.io/crates/chrono)
- [Officiell Rust-sajt](https://www.rust-lang.org)