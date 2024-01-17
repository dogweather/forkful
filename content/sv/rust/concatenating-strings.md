---
title:                "Sammanslående av strängar"
html_title:           "Rust: Sammanslående av strängar"
simple_title:         "Sammanslående av strängar"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konkatenera strängar är när man slår ihop flera strängar till en enda. Det är en vanlig uppgift som programmerare stöter på när de arbetar med textmanipulation. Det kan vara användbart för att skapa dynamiska meddelanden eller för att lägga till variabler i en sträng.

## Hur man gör:
Det finns flera sätt att konkatenera strängar i Rust. Ett sätt är att använda operatorn ```+```. Se exempel nedan:

```Rust
let first_name = "Peter";
let last_name = "Parker";

let full_name = first_name + " " + last_name;
println!("Hej, mitt namn är {}", full_name);
// Resultat: Hej, mitt namn är Peter Parker
```

Ett annat sätt är att använda makron ```format!``` eller ```format!("{sträng}", variabler)```. Detta gör det möjligt att sätta in variabler i strängen på specifika platser. Se exempel nedan:

```Rust
let first_name = "Peter";
let last_name = "Parker";

let full_name = format!("Hej, mitt namn är {} {}", first_name, last_name);
println!("{}", full_name);
// Resultat: Hej, mitt namn är Peter Parker
```

## Djupdykning:
Att konkatenera strängar har varit en vanlig uppgift för programmerare sedan de första programmeringsspråken utvecklades. I vissa språk, som C, behöver man använda en separat funktion för att konkatenera strängar, vilket kan leda till en längre och mer svårförståelig kod. I Rust, däremot, är det lättare och mer intuitivt att konkatenera strängar med hjälp av operatorer eller makron.

## Se även:
- Rust dokumentation om strängar: https://doc.rust-lang.org/std/string/index.html
- Mer om format makron: https://doc.rust-lang.org/book/ch08-02-strings.html#format-makros