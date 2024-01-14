---
title:                "Rust: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att läsa kommando radargument är en viktig del av att skriva effektiva Rust-program. Genom att kunna läsa argument från terminalen kan du skapa program som är mer anpassningsbara för användaren.

## Så här

Att läsa kommando radargument i Rust är enkelt och kan göras med hjälp av standard biblioteket. Här är ett exempel på hur du kan läsa ett argument som representerar en filnamn och skriva ut det till terminalen:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    // args[0] är alltid programnamnet
    // args[1] är första argumentet som användaren anger
    println!("Du har angett filnamnet: {}", args[1]);
}
```

Om du kör detta program med kommandot `cargo run filnamn.txt`, kommer följande att skrivas ut i terminalen:

```
Du har angett filnamnet: filnamn.txt
```

Du kan också använda en `match`-sats för att hantera olika scenario och argument. Till exempel, om du endast vill att programmet ska köras om användaren anger ett argument kan du göra följande:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        // För många argument, avbryt programmet
        1 => println!("Ange ett filnamn för att köra programmet."),
        // Endast ett argument anges, fortsätt med programmet
        2 => {
            println!("Du har angett filnamnet: {}", args[1]);
            // Din kod här
        }
        // Fler än två argument, avbryt programmet
        _ => println!("Endast ett argument kan anges."),
    }
}
```

## Djupdykning

Om du vill läsa flera argument eller vill hantera argument med olika typer, kan du använda `clap`-paketet. Det ger en enkel och konfigurerbar lösning för att läsa kommando radargument.

För att installera `clap`, lägg till följande i din `Cargo.toml`-fil:

```toml
[dependencies]
clap = "2.33.0"
```

Du kan sedan läsa kommando radargument på följande sätt:

```Rust
use clap::{App, Arg};

fn main() {
    // Skapa en ny app med hjälp av clap
    let matches = App::new("Min Rust-applikation")
        .version("1.0")
        .author("Ditt namn")
        .about("Ett program för att läsa kommando radargument.")
        // Lägg till de argument som ditt program behöver
        .arg(
            Arg::with_name("filnamn")
                .long("fil")
                .required(true)
                .takes_value(true),
        )
        .get_matches();

    // Hämta värdet för argumentet "filnamn"
    let filnamn = matches.value_of("filnamn").unwrap();

    println!("Du har angett filnamnet: {}", filnamn);
}
```

## Se även

[Bryce Mankin's blogg om kommando radargument i Rust](https://blog.burntsushi.net/rust-cli-framework-design/)

[Rust dokumentation för att läsa miljövariabler](https://doc.rust-lang.org/std/env/fn.var.html)

[Kompletta handledningar för att läsa från kommando raden i andra programmeringsspråk](https://ss64.com/bash/syntax-cmdline.html)