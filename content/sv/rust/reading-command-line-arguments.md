---
title:    "Rust: Läsning av kommandoradsargument"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Varför

Om du är ny till programmering i Rust eller bara behöver en påminnelse, kan det vara fördelaktigt att lära dig hur man läser kommandoradsargument. Det är en viktig del av programmering eftersom många program behöver läsa in användarinput för att fungera korrekt.

## Hur man läser kommandoradsargument i Rust

För att läsa kommandoradsargument i Rust, behöver vi använda en funktion som heter `args()` från standardbiblioteket. Denna funktion returnerar en vector med strängar som motsvarar de argument som användaren skrev in. Låt oss ta en titt på ett exempel:

```Rust
use std::env;

fn main() {
    // Läs in kommandoradsargumenten
    let args: Vec<String> = env::args().collect();
    
    // Skriv ut alla argument utom det första (som är programnamnet)
    for arg in args.iter().skip(1) {
        println!("{}", arg);
    }
}
```

Om vi kör detta program med argumenten `rust blogg post`, kommer output att vara:

```
blogg
post
```

## Djupdykning

För att få en bättre förståelse för hur funktionen `args()` fungerar, låt oss titta på dess signatur:

```
fn args() -> Args
```

Funktionen returnerar en typ som heter `Args`, som är en iterator över argumenten. Detta innebär att vi kan använda andra funktioner som finns på iteratoren, som `map()` och `filter()`, för att bearbeta argument på olika sätt. Om du vill läsa mer om iteratorer i Rust, kan du kolla in [denna artikel](https://doc.rust-lang.org/std/iter/).

## Se även

- [Dokumentation för funktionen `args()`](https://doc.rust-lang.org/std/env/fn.args.html)
- [Officiell Rust sida](https://www.rust-lang.org/sv-SE/)
- [Rust by Example: Command Line Arguments](https://doc.rust-lang.org/rust-by-example/std_misc/arg.html)