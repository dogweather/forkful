---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Generera slumpmässiga nummer handlar om att producera nummer som är oförutsägbara och icke-repetitiva. Programmerare genererar slumpmässiga tal för att simulera data, åstadkomma unika ID:er, eller för att skapa varierade resultat i spel och simuleringar.

## Hur man gör:

För att generera ett slumpmässigt heltal i Rust, kan vi använda `rand::Rng` trait och `rand::thread_rng()` funktionen. Se exemplet nedan.

```Rust
extern crate rand;
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    let numb: i32 = rng.gen();
    println!("Random number: {}", numb);
}
```
Kör det här programmet och du kommer bot få ett slumpmässigt nummer.

## Fördjupa sig:

Historiskt sätt, att generera slumpmässiga tal i programmering var faktiskt svårt på grund av datorernas deterministiska natur. Metoder för att skapa dessa "slumpmässiga" nummer inkluderade användning av tidpunkter (som det nuvarande tid och datum), tangenttryckningar eller musrörelser.

Rust ger oss alternativen `rand::thread_rng()` och `rand::random()` för att generera slumpmässiga nummer. `rand::thread_rng()` är en trådsäker generator som är lokal till den nuvarande tråden. `rand::random()` är ett enkelt sätt att producera ett slumpmässigt tal, men det har inte trådsäkerhet.

Implementationen av slumpmässig nummergenerering i Rust är baserat på en algoritmen Xorshift RNGs. Detta är en klass av pseudoslumpmässiga nummergeneratorer som har fördelen av att vara snabb och att använda liten mängd minne.

## Se även:

För mer detaljer och guider om antalet generering och programmering i Rust, se följande resurser:

- [Rust Dokumentation för `rand::Rng` trait](https://doc.rust-lang.org/rand/rand/trait.Rng.html)
- [Rust by Example, Kapitel 15.2: Genrer](https://doc.rust-lang.org/rust-by-example/std_misc/rand.html)
- [Wikipedia: Slumpmässiga Nummer Generering](https://en.wikipedia.org/wiki/Random_number_generation)