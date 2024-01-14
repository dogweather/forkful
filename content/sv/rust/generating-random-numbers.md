---
title:                "Rust: Skapa slumpmässiga tal"
programming_language: "Rust"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är en viktig del av många programmeringsprojekt, oavsett om det är för att skapa slumpmässiga spel, simuleringsprogram eller för att utföra statistisk analys. Med Rusts inbyggda randomiseringsverktyg kan du enkelt generera pålitliga och slumpmässiga nummer för ditt projekt.

## Hur man gör det

Att generera slumpmässiga nummer i Rust är enkelt. Först måste du importera biblioteket "rand". Sedan kan du använda funktionen "thread_rng()" för att skapa en trådsäker generator och sedan använda funktionen "gen_range()" för att generera ett slumpmässigt nummer inom ett angivet intervall.

```Rust
use rand::prelude::*; // Importera rand biblioteket

let mut rng = thread_rng(); // Skapa en trådsäker generator

let num = rng.gen_range(1..=100); // Generera ett slumpmässigt tal mellan 1 och 100

println!("Det slumpmässiga talet är: {}", num); // Skriv ut det slumpmässiga numret
```

Output:
```
Det slumpmässiga talet är: 57
```

## Djupdykning

Den mångsidiga "rand" biblioteket i Rust ger flera alternativ för att generera slumpmässiga nummer. Du kan till exempel använda funktionen "gen()" för att generera en slumpmässig bool (true/false), funktionen "gen_bool()" för att generera ett slumpmässigt flyttal, och funktionen "gen_ascii_chars()" för att generera slumpmässiga ASCII-tecken.

För att kontrollera hur slumpmässiga dina genererade nummer är, kan du använda funktionen "gen_seedable()" tillsammans med en "SeedableRng" för att skapa en generare med en specifik utgångspunkt (seed). Detta gör att du kan återskapa samma sekvens av slumpmässiga nummer vid varje körning av ditt program.

## Se även

- Rust dokumentation för randomiseringsbiblioteket:https://docs.rs/rand/0.6.5/rand/

- Att använda slumpmässighet i spelutveckling i Rust: https://rustgamedev.com/creating-a-random-number-generator-in-rust/

- Användning av slumpmässiga tal för simuleringar i Rust: https://vitiral.github.io/2018/01/07/Rust-Random-with-Std-Thread-Rng.html