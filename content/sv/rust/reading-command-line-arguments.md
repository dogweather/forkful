---
title:                "Rust: Läsning av kommandoradsargument"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att läsa in kommandoradsargument är en viktig del av programmeringsprocessen för många som använder sig av Rust. Det ger möjlighet att dynamiskt ändra programbeteendet utan att behöva ändra källkoden. Läs vidare för att ta reda på hur du kan implementera detta i dina egna projekt.

## Hur man gör

För att läsa in kommandoradsargument i din Rust-kod behöver du först importera biblioteket `std::env` genom att lägga till följande kod i början av din fil:

```Rust
use std::env;
```

Sedan kan du använda funktionen `args()` inom `env`-biblioteket för att få en vektor med alla kommandoradsargument som skickats till ditt program. Här är ett exempel på hur du kan skriva ut argumenten till konsolen:

```Rust
fn main() {
    let args: Vec<String> = env::args().collect();

    // Skriver ut argumenten till konsolen
    println!("Kommandoradsargument: {:?}", args);
}
```

Om du till exempel kör detta program med kommandot `rustc hello.rs Hej Världen` så kommer det att skriva ut `Kommandoradsargument: ["Hej", "Världen"]` till konsolen.

## Djupdykning

Det finns många olika sätt att hantera kommandoradsargument i Rust, men en vanlig teknik är att använda match uttryck för att hantera olika fall beroende på antal och innehåll av argument. Här är ett annat exempel som visar hur detta kan implementeras:

```Rust
fn main() {
    let args: Vec<String> = env::args().collect();

    // Match uttryck för att hantera olika fall
    match args.len() {
        // Om inga argument skickas till programmet
        1 => println!("Inga argument skickades."),
        // Om ett argument skickas till programmet
        2 => println!("Bara ett argument skickades: {}", args[1]),
        // Om flera argument skickas till programmet
        _ => println!("Flera argument skickades: {:?}", &args[1..]),
    }
}
```

Om du till exempel kör programmet med kommandot `rustc hello.rs Hej Världen` så kommer det att skriva ut `Flera argument skickades: ["Hej", "Världen"]`.

## Se även

* [Officiell dokumentation för kommandoradsargument i Rust](https://doc.rust-lang.org/std/env/index.html)
* [En tutorial om hur man hanterar kommandoradsargument i Rust](https://dev.to/devsyntax/handling-command-line-arguments-in-rust-9gf)

Tack för att du läste! Hoppas detta hjälper dig att hantera kommandoradsargument på ett smidigt sätt i dina Rust-projekt.