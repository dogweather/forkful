---
title:                "Debug-output afdrukken"
aliases:
- /nl/rust/printing-debug-output/
date:                  2024-01-28T22:04:36.688891-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debug-output afdrukken"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/rust/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het afdrukken van debugoutput stelt je in staat om een kijkje te nemen in de staat van je programma zonder een volwaardige debugger. Het is snel, ongepolijst en perfect voor het traceren van lastige bugs wanneer je niet de vuurkracht van een specifieke debugtool nodig hebt.

## Hoe:

Om iets eenvoudigs te printen, gebruik `println!`. Als je een waarde voor debugging moet printen, komt `dbg!` goed van pas.

```Rust
fn main() {
    let mut vec = vec![1, 2, 3];
    
    // Basis printen
    println!("Hallo, Rustaceans!");

    // Debugformatting met println! gebruikmakend van `{:?}`
    println!("{:?}", vec);

    // Debuggen met `dbg!`, print naar stderr en retourneert de waarde
    dbg!(&vec);

    // Wijzigen van vec na het gebruik van `dbg!`
    vec.push(4);
    dbg!(vec);
}
```

Voorbeelduitvoer:

```
Hallo, Rustaceans!
[1, 2, 3]
[src/main.rs:9] &vec = [
    1,
    2,
    3,
]
[src/main.rs:13] vec = [
    1,
    2,
    3,
    4,
]
```

## Dieper ingaan

Het printen van debugoutput is al sinds de vroege dagen van programmeren een eenvoudige onderdeel. De eenvoud maakt het vaak de go-to keuze voor het snel diagnosticeren van problemen.

In Rust is `println!` geweldig voor het weergeven van gebruiksvriendelijke berichten. De magie komt met `dbg!`, geïntroduceerd in Rust 1.32, dat zowel de waarde als zijn locatie in de code print. Het output naar standaardfout (stderr), dus het zal niet mengen met de standaardoutput (stdout) en kan indien nodig apart worden omgeleid.

Voor complexe typen kun je de `Debug` trait afleiden om automatisch een formaat te creëren dat `println!` en `dbg!` kunnen gebruiken. Dat is wat de annotatie `#[derive(Debug)]` doet boven je structs en enums.

Wat betreft alternatieven, er bestaan degelijke loggers zoals `log` en `env_logger`, en als je meer gedetailleerde controle nodig hebt, overweeg dan een debugger zoals `gdb` of `lldb`, die werken met Rust via integraties zoals `rust-gdb` of `rust-lldb`.

## Zie ook

Voor meer over Rust's debugprinten en formatteeropties:

- Het Rust Boek over `println!` en Formatteren: https://doc.rust-lang.org/std/fmt/index.html
- De documentatie van de `dbg!` macro: https://doc.rust-lang.org/std/macro.dbg.html
- Officiële gids voor debuggen met `gdb` en `lldb`: https://rust-lang.github.io/rustup-components-history
- `log` crate voor een meer gestructureerde benadering van loggen: https://crates.io/crates/log
- `env_logger` crate, een veelgebruikte loggerimplementatie voor de `log` facade: https://crates.io/crates/env_logger
