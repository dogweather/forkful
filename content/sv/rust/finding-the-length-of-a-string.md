---
title:                "Rust: Att hitta längden på en sträng"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng kanske verkar som en enkel uppgift, men i programmering kan det vara mycket viktigt. Det kan hjälpa till att göra vår kod effektivare och minska risken för buggar.

## Så här gör du

Att hitta längden på en sträng i Rust är enkelt. Vi kan använda standard biblioteket för Rust som heter "std" och dess "string" modul. Inuti den här modulen finns funktionen "len" som tar emot en sträng som parameter och returnerar dess längd.

```Rust
let string = "Hej alla!"; // En sträng med längden av 9 tecken
let length = std::string::len(string); // Returnerar 9 
```

Det är viktigt att komma ihåg att strängar är kodade som UTF-8 i Rust, vilket innebär att en enskild tecken kan bestå av flera bytes och kommer påverka längden av strängen.

## Djupdykning

En intressant aspekt av att hitta längden på en sträng i Rust är att det faktiskt är en uppsättning av funktioner och inte bara en enda funktion. Förutom "len" finns det också "chars", "bytes" och "chars_len". Dessa funktioner returnerar längden av en sträng baserat på dess tecken, bytes eller unicode tecken.

En annan viktig sak att notera är att längden av en sträng i Rust är en "usize" typ, vilket betyder att det är beroende av maskinens arkitektur och kan variera på 32- och 64-bitars system.

## Se även

- [Officiell dokumentation för strängar i Rust](https://doc.rust-lang.org/std/string/index.html)
- [En översikt över UTF-8 i Rust](https://rolisz.ro/2016/02/09/utf8-in-rust-a-brief-overview/)
- [En djupdykning i Rusts "std" bibliotek](https://www.airpair.com/rust/posts/rust-standard-library-building-blocks)