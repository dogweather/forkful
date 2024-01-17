---
title:                "Generera slumpmässiga nummer"
html_title:           "Rust: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumpmässiga tal är en vanlig uppgift inom programmering. Det ger möjligheten att skapa unika värden för olika användningar, som spel, kryptering och testning av kod. 

## Så här gör man:
```Rust
use rand::Rng;

fn main() {
  // Skapa en ny slumpgenerator med hjälp av standardbiblioteket "rand"
  let mut rng = rand::thread_rng();
  
  // Generera slumpmässigt heltal mellan 1 och 100
  let random_number = rng.gen_range(1, 101);
  
  println!("Det slumpmässiga talet är: {}", random_number);
}
```
Exempeloutput:
`Det slumpmässiga talet är: 47`

## Djupdykning:
Att generera slumpmässiga tal har varit en utmaning inom programmering sedan tidigt 1900-tal. Tidigare användes frön från tiden för att skapa slumpmässiga sekvenser, men dessa kunde vara förutsägbara och repeteras. Med utvecklingen av moderna algoritmer och datorteknik, finns det nu bättre metoder för att skapa säkra slumpmässiga tal. 

En annan metod för att generera slumpmässiga tal är genom användning av hårdvarugenererade tal, vilket använder extern hårdvara för att skapa sekvenser av bitar som kan användas som slumpmässiga tal.

I Rust finns flera bibliotek för att generera slumpmässiga tal, såsom "rand" som användes i exemplet ovan. Det finns också möjligheten att skapa din egen slumpgenerator med hjälp av kryptografiska algoritmer som ger extra säkerhet för vissa applikationer.

## Se även:
- [Dokumentation för Rust's "rand" bibliotek](https://docs.rs/rand/)
- [Wikipedia artikel om generering av slumpmässiga tal](https://sv.wikipedia.org/wiki/Slumpgenerator)
- [Artikel om hårdvarugenererade slumpmässiga tal](https://www.eniac.com/the-basics-of-hardware-generated-random-numbers)