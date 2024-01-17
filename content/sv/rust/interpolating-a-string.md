---
title:                "Interpolerar en sträng"
html_title:           "Rust: Interpolerar en sträng"
simple_title:         "Interpolerar en sträng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Interpolering av en sträng i Rust är när man använder variabler eller uttryck inuti en sträng för att skapa en dynamisk sträng istället för en statisk. Detta gör det möjligt att bygga strängar som innehåller olika värden på olika platser och ofta används det för att skapa mer läsbara och ändringsbara strängar på ett enkelt sätt.

## Så här gör du:

```Rust
let name = "förnamn";
let age = 25;

println!("Hej, mitt namn är {} och jag är {} år gammal!", name, age);
```

Output:
```
Hej, mitt namn är förnamn och jag är 25 år gammal!
```

## Djupdykning:

Interpolering av strängar i programmering har funnits i många år och är vanlig i olika programmeringsspråk som till exempel Python och JavaScript. I Rust görs detta med hjälp av ett utropstecken följt av ett par parenteser. Alternativ till interpolering i Rust inkluderar konkatenering av strängar eller användning av format-makron. Implementationen av interpolering i Rust använder sig av makron för att ta in variablerna och ersätta rätt delar i strängen.

## Se även:

- [Rust Documentation on String Formatting](https://doc.rust-lang.org/std/fmt/)
- [Rust Programming Language](https://www.rust-lang.org/)