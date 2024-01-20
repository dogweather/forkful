---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Delsträng-extrahering är processen att dra ut en del av en sträng baserat på bestämda index (placeringar i strängen). Det är viktigt för programmerare eftersom det möjliggör manipulering och analys av specifika delar av stora strängar av data.

## Så här gör du:

Låt oss titta på några exempel med Rust:

```Rust
fn main() {
    let s = "Hej världen!";
    let delstrang_s = &s[0..3];
    println!("{}", delstrang_s);
}
```

Output:

```Rust
"Hej"
```

I exempel koden ovan, extraherar vi delsträngen "Hej" från strängen "Hej världen!" genom att specificera indexintervall[0..3].

## Djup Dykning

I historisk sammanhang har delsträngsextraktion alltid varit en kärnkomponent i strängmanipulation. I Rust, är det vanligtvis gjort med "slice" syntax, som påminner mycket om användning av index i matematiska intervall.

Några alternativ till den här metoden kan inkludera användning av bibliotek som `regex` för mer komplexa behov.

När det gäller genomförande, använder Rust en `byte_offset` funktion för indexering, vilket betyder att varje Unicode-kodpunkt räknas och i sin tur förklarar behovet av att specifiera både början och slutet av en subträng extraktion.

## Se Även

För mer djupgående lärande, se följande länkar:

1. [The Rust Book: The Slice Type](https://doc.rust-lang.org/book/ch04-03-slices.html#string-slices)
2. [Rust By Example: Primitives](https://doc.rust-lang.org/rust-by-example/primitives/tuples.html)
3. [Understanding String Slicing in Rust](https://www.forrestthewoods.com/blog/understanding-rust-string-string-slicing/)