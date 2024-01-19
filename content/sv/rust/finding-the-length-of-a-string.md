---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hitta längden på en sträng innebär att räkna antalet tecken i den. Programmerare gör detta för att hantera data effektivt, till exempel vid loopar, minnesallokering eller validering av användarinmatning.

## Hur man Gör:
Här är ett exempel på hur man får längden på en sträng i Rust:
```Rust
fn main() {
    let string = "Hej, världen!";
    println!("{}", string.len());
}
```
När du kör den här koden får du outputet `14`, vilket är antalet tecken i strängen, inklusive blanksteg och interpunktion.

## Djupdykning
Historiskt sett behandlar olika programmeringsspråk stränglängder olika. I C++, till exempel, inkluderar stränglängd inte nolltecknet vid slutet av en sträng. Rust, däremot, räknar alla tecken i en sträng, inklusive blanksteg och interpunktion.

Ett alternativ till `len()` metoden i Rust är att använda `chars().count()`, vilket ger samma resultat men kan vara mindre effektivt för stora strängar. Skillnaden ligger i implementationen: `len()` returnerar direkt antalet byte i strängen, medan `chars().count()` itererar över varje tecken.

## Se Även
Mer information om detta ämne finns i Rusts officiella dokumentation. Här är några nyttiga länkar:

- Strängtyper i Rust: [Rust Docs String](https://doc.rust-lang.org/std/string/index.html)
- Metoder för att manipulera strängar: [Rust Docs str](https://doc.rust-lang.org/std/primitive.str.html)
- Guiden till Rusts standardbibliotek: [Rust Standard Library](https://doc.rust-lang.org/std/)
Om du vill ha en uttömmande guide till Rust, kan du läsa [The Rust Programming Language](https://doc.rust-lang.org/book/) - en gratis, officiell bok om Rust.