---
title:    "Rust: Läsning av kommandoradsargument"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Varför läsa kommandoradsargument i Rust?

Att kunna läsa och hantera kommandoradsargument är en viktig färdighet för många programmerare, oavsett vilket språk de använder. I denna artikel kommer vi att utforska hur man läser kommandoradsargument i Rust och hur det kan hjälpa till att förbättra ditt programmeringsarbete.

## Hur man läser kommandoradsargument i Rust

Att läsa kommandoradsargument i Rust är enkelt och kan göras med hjälp av standardbiblioteket `std::env`. Vi kan använda funktionen `args` för att få en vector av de argument som användaren skickade med vid körningen av vårt program. Här är ett enkelt exempel:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    // Vi kan nu använda vector:n args för att hämta de argument som skickades med i kommandoraden
    for argument in &args {
        println!("{}", argument);
    }
}
```

Om vi nu exekverar vårt program med kommandoradsargumenten `rust program.rs arg1 arg2`, kommer vi få följande utskrift:

```
arg1
arg2
```

## DJupdykning i kommandoradsargument

Förutom att läsa och skriva ut kommandoradsargument, kan vi också utföra olika operationer på dem. Det finns till exempel inbyggda funktioner för att söka efter specifika argument eller att konvertera dem till andra datatyper. Här är några exempel på hur vi kan använda dessa funktioner:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    // Omvandla argumentet till en Integer
    let num: i32 = args[1].parse().expect("Argument måste vara ett nummer!");

    // Kontrollera om argumentet är en flagga eller inte
    if args[2].starts_with("-") {
        println!("Argument 2 är en flagga: {}", args[2]);
    } else {
        println!("Inget argument hittades");
    }
}
```

Med dessa funktioner kan du skräddarsy ditt program för att ta emot och hantera specifika kommandoradsargument som passar dina behov.

# Se också

Om du vill lära dig mer om hur du hanterar kommandoradsargument i Rust, rekommenderar vi att du läser följande dokumentation och guider:

- [Den officiella dokumentationen för std::env](https://doc.rust-lang.org/std/env/index.html)
- [En guide om hur man läser kommandoradsargument i Rust](https://www.geeksforgeeks.org/command-line-arguments-in-rust-programming/)

Med dessa resurser och din nya kunskap om hur man läser kommandoradsargument i Rust, kan du ta dina programmeringsfärdigheter till nästa nivå. Lycka till och ha kul med dina projekt!