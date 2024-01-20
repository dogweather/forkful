---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

# Rust-programmering: Så här skriver du ut felsökningsresultat ("Debug Output")

## Vad & Varför?
Att skriva ut felsökningsresultat är processen att visa interna datakällor i koden för felsökning. Programmerare gör detta för att spåra och lösa buggar effektivt.

## Hur man gör:

Använd `println!` och `{:?}` eller `{:#?}` för att skriva ut felsökningsresultat i Rust. Exempel:

```Rust
fn main() {
    let v = vec![1, 2, 3];
    println!("Debug: {:?}", v);
    println!("Pretty Debug: {:#?}", v);
}
```

Detta kommer att skriva ut:

```Rust
Debug: [1, 2, 3]
Pretty Debug: [
    1,
    2,
    3,
]
```

## Djupdykning

1. Historisk context: Rust introducerade `{:?}` för att skriva ut Debug-trait i stället för `fmt` gränssnittet, vilket ger en mer detaljerad utförlig utskrift.
2. Alternativ: Utöver `println!` kan du också använda `debug!` från loggbiblioteket för att kontrollsystemets loggnivåer.
3. Implementeringsdetaljer: Var medveten om att inte alla typer kan skrivas ut med `{:?}`. Din typ behöver implementera Debug-traiten.

## Se också

- [Rusts dokumentation för `fmt` ](https://doc.rust-lang.org/std/fmt/)
- [Loggbiblioteket `log`](https://docs.rs/log/0.4.14/log/) 

Kom ihåg, skriv ut felsökningsresultat hjälper dig att lättare hitta och lösa problem i din kod. Använd denna teknik klokt och effektivt för din Rust-programmering.