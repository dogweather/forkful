---
title:    "Rust: Kapitalisering av en sträng"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Varför

Att dela upp denna kodpost i tre delar med fokus på kapitalisering av strängar i Rust, är för att ge läsarna en djupare förståelse för hur och varför man bör använda sig av denna funktion i sitt programmeringsarbete. Det kan vara ett användbart verktyg för att förbättra användbarheten och läsbarheten i dina program.

## Hur man gör det

För att börja använda funktionen för att kapitalisera strängar i Rust, behöver du först importera detta bibliotek i ditt program:

```Rust
use std::string::String;
```

Sedan kan du använda den inbyggda metoden `.to_uppercase()` för att kapitalisera en sträng:

```Rust
let my_string = "hej hej".to_uppercase();
println!("{}", my_string);
```

Output:

```Rust
HEJ HEJ 
```

Den här metoden kan också användas på variabler, såsom i följande exempel:

```Rust
let initial = "jag gillar rust";
let changed = initial.to_uppercase();
println!("{} men jag älskar att programmera!", changed);
```

Output:

```Rust
JAG GILLAR RUST men jag älskar att programmera!
```

## Djupdykning

Att förstå hur man kapitaliserar strängar i Rust är en viktig del av att arbeta med strängar. Denna funktion kan vara särskilt användbar vid att inkludera användarinput i dina program, där det är viktigt att formatera och visa data på ett lättläst sätt.

Det är också värt att notera att `to_uppercase()` är en icke-destruktiv metod, vilket betyder att den inte ändrar på den ursprungliga strängen, utan returnerar en ny kapitaliserad version av den.

## Se också

Här är några länkar för vidare läsning om strängmanipulering i Rust:

- [Officiell dokumentation för strängar i Rust](https://doc.rust-lang.org/std/string/index.html)
- [En guide till hur man arbetar med strängar i Rust](https://stevedonovan.github.io/rust-gentle-intro/2-strings.html)
- [Fler exempel på inbyggda strängfunktioner i Rust](https://www.geeksforgeeks.org/rust-string/)

Tack för att du läste!