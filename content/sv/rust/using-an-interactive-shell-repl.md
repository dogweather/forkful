---
title:                "Använda en interaktiv skal (REPL)"
date:                  2024-01-26T04:18:01.401756-07:00
model:                 gpt-4-0125-preview
simple_title:         "Använda en interaktiv skal (REPL)"

category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Vad & Varför?
En interaktiv Rust-skal, eller REPL (Read-Eval-Print Loop), låter dig köra Rust-kod på flygande fot och se omedelbara resultat, perfekt för experiment eller lärande. Programmerare använder det för att testa kodsnuttar, felsöka eller bara leka med språkets funktioner utan overhead av att kompilera ett helt projekt.

## Hur man gör:
För tillfället har Rust inte en officiell REPL som medföljer. Du kan använda tredjepartverktyg som `evcxr_repl`. Installera det med Cargo:

```sh
cargo install evcxr_repl
```

Sedan kör du REPL:

```sh
evcxr
```

Inuti, testa lite Rust-kod:

```rust
let x = 5;
let y = 3;
println!("{} + {} = {}", x, y, x + y);
```

Utskriften bör vara:

```
5 + 3 = 8
```

## Fördjupning
Rusts ethos är centrerat kring säkerhet och prestanda, vilka vanligtvis associeras med språk som kompileras i förväg, och mindre med tolkade, REPL-vänliga sådana. Historiskt sett prioriterade språk som Python eller Ruby att ha en REPL för omedelbar feedback, men var inte designade med systemnivå-uppgifter i åtanke.

Trots frånvaron av en officiell REPL i Rust, har ett par alternativ som `evcxr_repl` uppstått. Dessa projekt hackar inte bara in Rust i en REPL; de flätar intelligent ihop språkets kompilera-och-köra cykel till en interaktiv session. REPL kompilerar koden bakom kulisserna och kör binärfilen, fångar utskriften. På detta sätt bevarar den Rusts fördelar med prestanda samtidigt som den ger den interaktiva upplevelsen.

Det pågår löpande diskussioner inom Rustgemenskapen om officiellt REPL-stöd, och med varje språkiteration ser vi mer sofistikerade verktyg som så småningom kan leda till en nativ lösning.

## Se även
För mer info och andra verktyg:
- Evcxr REPL GitHub repo: [https://github.com/google/evcxr](https://github.com/google/evcxr)
- Rust Playground, ett online sätt att testa Rust-kod: [https://play.rust-lang.org/](https://play.rust-lang.org/)
- Rust-språkdiskussion om REPL-funktion: [https://internals.rust-lang.org/](https://internals.rust-lang.org/)
