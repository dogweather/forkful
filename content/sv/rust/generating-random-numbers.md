---
title:    "Rust: Generering av slumpmässiga tal"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är en viktig del av många programmeringsprojekt. Det kan användas för allt från spel och lotteridragningar till kryptering och simuleringar.

## Hur man gör det

Att generera slumpmässiga nummer i Rust är enkelt med hjälp av det inbyggda "rand" biblioteket. Vi kan använda "rand::prelude" för att importera nödvändiga funktioner och sedan använda funktionen "thread_rng" för att skapa en generator. Här är ett exempel på hur man genererar en slumpmässig integer mellan 1 och 100:

```
Rust
use rand::prelude::*;
 
let mut rng = thread_rng();
let random_number = rng.gen_range(1, 101);
 
println!("Det slumpmässiga numret är: {}", random_number);
```
Output: Det slumpmässiga numret är 66

Vi kan också generera en slumpmässig boolean genom att använda funktionen "gen::<bool>()" eller generera en slumpmässig float mellan 0 och 1 genom att använda "gen_range::<f64>(0.0, 1.0)". Det finns många andra möjligheter med "rand" biblioteket, så se till att utforska det för att hitta den bästa lösningen för ditt specifika projekt.

## Djupdykning

Att generera slumpmässiga nummer är en viktig del av många datorkonstruktioner, inklusive krypteringsalgoritmer och simuleringar. Det är också viktigt att förstå att datorer inte kan generera "verkliga" slumpmässiga nummer eftersom det kräver ett yttre element av kaos. Istället använder programmeringsspråk olika algoritmer för att skapa ett mönster av nummer som kan utgöra slumpmässiga nummer. Därför är det alltid viktigt att veta vilken algoritm som används för att generera slumpmässiga nummer i ett specifikt program.

## Se också

- [Dokumentation för "rand" biblioteket](https://docs.rs/rand/0.8.3/rand/)
- [Mer information om hur datorer genererar slumpmässiga nummer](https://www.random.org/randomness/)
- [Illustrerade exempel på algoritmer för slumpmässiga nummer](https://www.geeksforgeeks.org/generating-random-number-range-c/)