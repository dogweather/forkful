---
title:                "Skriva ut felsökningsdata"
aliases: - /sv/rust/printing-debug-output.md
date:                  2024-01-20T17:53:18.311526-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skriva ut felsökningsdata"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Utskrift för felsökning hjälper programmerare att förstå vad deras kod gör under körning. Det gör att vi kan spåra värden och programflöde för att hitta och fixa buggar.

## Hur man gör:
För att skriva ut debugginfo i Rust, använd `println!` med debug-flaggan `{:?}`:

```Rust
fn main() {
    let my_data = vec![1, 2, 3];
    println!("Debug-info: {:?}", my_data);
}
```

Utskrift skulle se ut så här:

```
Debug-info: [1, 2, 3]
```

Om du vill ha en mer detaljerad och formaterad utskrift, kan du använda `#` tillsammans med debug-flaggan:

```Rust
fn main() {
    let my_data = vec![1, 2, 3];
    println!("Detaljerad debug-info: {:#?}", my_data);
}
```

Det skulle ge denna detaljerade utskrift:

```
Detaljerad debug-info: [
    1,
    2,
    3,
]
```

Kom ihåg att din typ måste implementera `Debug` trait för att använda `{:?}` eller `{:#?}`.

## Deep Dive
I Rusts barndom insåg man att att skriva ut debug-information var kritiskt, så `Debug` trait implementerades tidigt i språkets utveckling. Det finns alternativ till `println!`, såsom att logga med bibliotek som `log` eller `env_logger` för mer kontroll och förmågan att hantera olika loggnivåer.

`Display` är en annan trait som liknar `Debug`, men den är avsedd för användarvänlig utskrift snarare än debug-utskrift. `Display` behöver en mer manuell implementering där du bestämmer formatet, medan `Debug` oftast kan använda en derivat-deklaration för automatisk implementering.

`std::fmt` modulen i Rust standardbiblioteket hanterar formattering. Det finns många andra traits i denna modul som kan användas för att anpassa utskrift utöver `Debug` och `Display`.

## Se även
- Rusts officiella dokumentation för `std::fmt`: https://doc.rust-lang.org/std/fmt/
- Rust By Example om Custom Types och Debug: https://doc.rust-lang.org/stable/rust-by-example/hello/print/print_debug.html
- `log` crate: https://crates.io/crates/log
- `env_logger` crate: https://crates.io/crates/env_logger
