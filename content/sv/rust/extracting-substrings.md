---
title:                "Utvinna substrängar"
html_title:           "Rust: Utvinna substrängar"
simple_title:         "Utvinna substrängar"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att extrahera substrängar är en process där en delsträng av en större sträng tas ut och används separat. Programmörer använder sig ofta av detta för att hantera och manipulera data på ett effektivt sätt.

## Så här gör du:
Exempel på hur man extraherar substrängar i Rust, tillsammans med förväntat resultat:
```rust
let str = "Hej världen!";
let substr = &str[4..9]; // extraherar "värld"
println!("{}", substr); // förväntat resultat: "värld"
```
```rust
let str = "Rust är bäst!";
let substr = &str[0..4]; // extraherar "Rust"
println!("{}", substr); // förväntat resultat: "Rust"
```

## Djupdykning:
Extrahering av substrängar har funnits med sedan de tidiga versionerna av programmeringsspråket Rust. Det finns också andra sätt att extrahera substrängar, till exempel genom att använda metoder som "split" och "slice". Implementeringen av detta skiljer sig också beroende på vilken datatyp som används för substrängen.

## Se även:
- [Rust String Documentation](https://doc.rust-lang.org/std/string/)
- [Rust Slice Documentation](https://doc.rust-lang.org/std/primitive.slice.html)
- [Rust Split Method Documentation](https://doc.rust-lang.org/std/primitive.str.html#method.split)