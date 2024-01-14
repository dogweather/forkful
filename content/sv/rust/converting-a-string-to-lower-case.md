---
title:    "Rust: Konvertera en sträng till små bokstäver"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener (lower case) kan vara en vanlig uppgift när man arbetar med textbaserade applikationer eller webbutveckling. Genom att använda Rusts inbyggda funktioner för stränghantering kan man enkelt omvandla en sträng till gemener. Nedan kan du lära dig hur du gör det.

## Så här gör du

För att konvertera en sträng till gemener i Rust, behöver du först importera standardbiblioteket "str". Därefter kan du använda funktionen "to_lowercase()", som tillämpas på en variabel som innehåller en sträng. Exempelvis:

```Rust
use std::str;

let text = "HEJ HEJ";

let gemener = str::to_lowercase(&text);

println!("{}", gemener);
```

Output: hej hej

Som du kan se, använder vi funktionen "to_lowercase()" för att omvandla vår variabel "text" till gemener och sedan skriver ut resultatet. Det är så enkelt det är att konvertera en sträng till gemener i Rust!

## Djupdykning

Vad händer egentligen när vi använder funktionen "to_lowercase()"? Först och främst utför den en iteration genom varje tecken i strängen och kontrollerar om det är ett stort bokstav genom att använda "is_uppercase()" funktionen. Om tecknet i fråga är en stor bokstav så konverteras det till gemener genom att subtrahera 32 från dess ASCII-värde. Därefter lagras den omvandlade strängen i en ny variabel och returneras till användaren.

## Se även

- [Rust String Library](https://doc.rust-lang.org/std/string/)
- [ASCII Table](https://www.rapidtables.com/code/text/ascii-table.html)