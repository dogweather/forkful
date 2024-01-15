---
title:                "Läsning av kommandolinjeargument"
html_title:           "Rust: Läsning av kommandolinjeargument"
simple_title:         "Läsning av kommandolinjeargument"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att läsa kommandoradsargument i Rust är ett viktigt steg för att skapa mer interaktiva program. Genom att hantera kommandoradsargument kan ditt program ta in information från användaren, vilket ger en mer dynamisk och anpassningsbar upplevelse.

## Så här gör du

För att läsa kommandoradsargument i Rust, behöver du importera biblioteket "std" och funktionen "env" som är en del av detta bibliotek. Detta ger tillgång till "args" vilket är en vektor som innehåller alla kommandoradsargument som har passerats till ditt program.

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("Kommandoradsargumenten är: {:?}", args);
}
```

Om du till exempel kör detta program med kommandoradsargumenten "hello world", kommer outputen att bli:

```
Kommandoradsargumenten är: ["hello", "world"]
```

Nu kan du använda detta argument i ditt program för att göra olika saker beroende på vad användaren matat in.

## Deep Dive

För att fördjupa dina kunskaper om att läsa kommandoradsargument i Rust, kan det vara bra att förstå hur vektorn "args" fungerar. Argumenten lagras i vektorn i samma ordning som de matats in på kommandoraden. Det första argumentet ligger alltså på index 0, det andra på index 1 osv. Om inga argument matats in, kommer vektorn bara att innehålla ett element som är namnet på ditt program.

Det kan också vara användbart att veta att "args" även innehåller det första elementet som är namnet på ditt program. Detta kan dock enkelt tas bort med hjälp av funktionen "shift" som tar bort det första elementet i vektorn och returnerar det.

## Se även

- [Officiell Rust dokumentation för Command Line Arguments](https://doc.rust-lang.org/std/env/fn.args.html)
- [En tutorial om att använda Command Line Arguments i Rust](https://www.tutorialspoint.com/rust/rust_program_arguments.htm)