---
title:                "Kapitalisering av en sträng"
html_title:           "Rust: Kapitalisering av en sträng"
simple_title:         "Kapitalisering av en sträng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När vi talar om att "kapitalisera en sträng" i programmering, menar vi att göra den första bokstaven i varje ord i strängen stor. Detta beteende är vanligtvis önskvärt för att göra strängar mer lättlästa och konsekventa. 

## Hur man:
Kapitalisera en sträng i Rust är enkelt. Använd bara funktionen `to_uppercase()` på en sträng och få tillbaka en ny sträng med enbart stora bokstäver. Till exempel:
```Rust
let min_sträng = String::from("hej, vem är du?");
let kapitaliserad = min_sträng.to_uppercase();
println!("{}", kapitaliserad);
```
Output: HEJ, VEM ÄR DU?

Funktionen `to_uppercase()` fungerar på både engelska och icke-engelska bokstäver.

## Djupdykning:
Att kapitalisera strängar är ett vanligt sätt att förbättra läsbarheten och konsistensen i program. Innan Unicode fick man ofta använda en funktion som kallades "toupper" för att konvertera till stora bokstäver, men detta fungerade endast på engelska bokstäver och ignorerade specialtecken. Numera med Unicode är `to_uppercase()` en mycket mer användbar och tillförlitlig lösning på detta problem.

## Se även:
- Rust's officiella dokumentation för `to_uppercase()`: https://doc.rust-lang.org/std/string/struct.String.html#method.to_uppercase
- En detaljerad beskrivning av `to_uppercase()` funktionen: https://stackoverflow.com/questions/53067205/rust-string-to-uppercase
- En tutorial om olika sätt att manipulera strängar i Rust: https://blog.logrocket.com/10-types-of-string-manipulation-techniques-with-examples-in-rust/