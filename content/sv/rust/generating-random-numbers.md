---
title:                "Generering av slumpmässiga tal"
html_title:           "Rust: Generering av slumpmässiga tal"
simple_title:         "Generering av slumpmässiga tal"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Varför
Att generera slumpmässiga nummer är en viktig funktion inom programmering. Det kan användas för att skapa variationsrika spel, göra slumpartade val eller testa program i olika scenarion. I denna artikel kommer vi att titta på hur man kan göra detta med hjälp av Rusts inbyggda funktioner.

# Så här
För att generera slumpmässiga nummer i Rust behöver vi använda oss av biblioteket `rand`. För att lägga till detta bibliotek i vårt projekt, måste vi först lägga till det som en dependens i vår `Cargo.toml` fil:
```Rust
[dependencies]
rand = "0.6.5"
```

Efter att ha lagt till biblioteket kan vi använda det i vårt program:
```Rust
use rand::Rng;
```
Nu har vi tillgång till alla funktioner som behövs för att generera slumpmässiga nummer.

För att generera ett slumpmässigt heltal, kan vi använda funktionen `gen_range`:
```Rust
let random_number = rand::thread_rng().gen_range(1..101);
// Genererar ett slumpmässigt heltal mellan 1 och 100
```

För att skapa en slumpmässig flyttalsvariabel, kan vi använda funktionen `gen`:
```Rust
let random_float = rand::thread_rng().gen::<f64>();
// Genererar ett slumpmässigt flyttal
```

Om vi vill ha en sträng med slumpmässiga tecken, kan vi använda funktionen `gen_ascii_chars`:
```Rust
let random_string: String = rand::thread_rng().sample_iter(rand::distributions::Alphanumeric).take(10).collect();
// Genererar en sträng med 10 slumpmässiga alfanumeriska tecken
```

# Djupdykning
Biblioteket `rand` använder sig av en pseudo-slumpmässig generator, vilket innebär att den inte genererar verkligt slumpmässiga nummer. Istället använder den ett startvärde (seed) för att skapa en sekvens av nummer som kan verka slumpmässig vid användning. Detta seed kan antingen sättas av användaren eller genereras av operativsystemet.

För att se mer information om hur den slumpmässiga generatorn fungerar, kan man titta på källkoden för `rand`, som är öppen och tillgänglig på Github.

# Se även
- **Rust Core Documentation**: https://doc.rust-lang.org/core/
- **Rust Standard Library Documentation**: https://doc.rust-lang.org/std/
- **Rand Crate Documentation**: https://docs.rs/rand/0.6.5/rand/

Tack för att du läste denna artikel om hur man genererar slumpmässiga nummer i Rust. Med hjälp av dessa funktioner och lite kreativitet, kan du skapa spännande program som utnyttjar slumpmässighet. Lycka till med dina programmeringsprojekt!