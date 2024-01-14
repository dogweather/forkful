---
title:                "Rust: Generering av slumpmässiga nummer"
simple_title:         "Generering av slumpmässiga nummer"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att kunna generera slumpmässiga nummer är en viktig del av många program och spel. Det tillåter oss att skapa variation och hjälper till att skapa en mer spännande användarupplevelse.

## Hur man gör det

```Rust
use rand::Rng;

fn main() {
    // För att generera ett slumpmässigt heltal mellan 1 och 100
    let num = rand::thread_rng().gen_range(1, 101);
    println!("Det slumpmässiga talet är: {}", num);
    
    // För att generera ett slumpmässigt flyttal mellan 0 och 1
    let float = rand::thread_rng().gen();
    println!("Det slumpmässiga flyttalet är: {}", float);
}
```

Output:

Det slumpmässiga talet är: 56
Det slumpmässiga flyttalet är: 0.03915819815607013

I detta exempel använder vi Rust biblioteket "rand" för att generera slumpmässiga nummer. För att göra detta behöver vi först importera "Rng" modulen. Sedan använder vi "thread_rng()" funktionen för att skapa en slumpmässig generator och "gen_range()" functionen för att ange vilket intervall numret ska vara mellan. Du kan läsa mer om de olika funktionerna och alternativen de har i dokumentationen för "rand" biblioteket.

Det är important att observera att resultaten i exemplet ovan kommer att vara olika varje gång koden körs, eftersom den genererar ett sant slumpmässigt nummer varje gång.

## Djupdykning

Bakom kulisserna använder "rand" biblioteket ett koncept som kallas för "pseudo-random nummer generering". Detta innebär att numren som genereras egentligen inte är helt slumpmässiga, men de är tillräckligt nära för att inte göra någon skillnad för de flesta användningsområden.

Detta är möjligt genom att använda en matematisk algoritm som tar ett startvärde, eller en "seed", och använder det för att beräkna nästa nummer i sekvensen. Så länge startvärdet är olika varje gång du kör koden, kommer den genererade sekvensen av nummer att vara unik varje gång.

## Se även

* [Dokumentation för rand biblioteket](https://docs.rs/rand/0.8.4/rand/)
* [En djupare förklaring om pseudo-random nummer generering](https://www.geeksforgeeks.org/pseudo-random-number-generator-prng/)