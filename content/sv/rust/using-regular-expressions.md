---
title:    "Rust: Användning av reguljära uttryck"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Varför
Att använda reguljära uttryck (regular expressions) är ett kraftfullt sätt att söka och manipulera textsträngar i din kod. Det kan hjälpa dig att effektivt hitta och bearbeta specifika mönster i en text. Om du vill lära dig mer om reguljära uttryck i samband med programmering i Rust, fortsätt läsa!

## Hur man gör
För att använda reguljära uttryck i Rust, måste du först importera biblioteket `regex`. Detta kan göras genom att lägga till `extern crate regex;` i din `main.rs` fil. Sedan kan du använda `Regex` structen för att skapa ett nytt reguljärt uttryckt med önskat mönster.

```Rust
use regex::Regex;

let text = "Hello, my name is Rust!";
let re = Regex::new(r"rust").unwrap();
let result = re.find(text);

println!("{:?}", result); // Output: Some(Match { start: 18, end: 22 })
```

I detta exempel skapar vi ett nytt reguljärt uttryck som söker efter ordet "rust" i en sträng. Vi använder sedan `find` funktionen för att hitta det första matchande mönstret. Om det finns en matchning, kommer `result` att returnera `Some(Match)` med start- och slutposition för matchningen. Om inte, returneras `None`.

## Djupdykning
När du arbetar med reguljära uttryck i Rust, kan det vara användbart att känna till några av de vanligaste metaskaraktärerna och regex funktionerna. Till exempel kan du använda `+` för att matcha ett eller flera förekomster av den tidigare uttryckta gruppen, eller `*` för noll eller flera förekomster. Andra metaskaraktärer som kan vara användbara inkluderar `^` för att matcha början av en sträng och `$` för att matcha slutet av en sträng.

Det finns också några användbara funktioner som `captures`, som returnerar en `Captures` struct för att arbeta med matchningar som innehåller fångade uttryck, och `replace`, som låter dig byta ut delar av en sträng med ett annat uttryck.

## Se även
* [Officiella Rust dokumentationen om reguljära uttryck](https://doc.rust-lang.org/1.30.0/std/regex/index.html)
* [En interaktiv reglujära uttryck lektion med Rust](https://rust-regex.github.io/regex-rust/book/)