---
title:                "Att göra en sträng versal"
html_title:           "Bash: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva med versaler innebär att förvandla alla bokstäver i en textsträng till stora bokstäver. Programmerare gör det för att särskilja text, som i titlar, eller för att standardisera data, exempelvis i användarnamn eller koder.

## Hur man gör:
```Rust
fn main() {
    let text = "rust är trevligt!";
    let capitalized_text = text.to_uppercase();
    
    println!("Original: {}", text);
    println!("Med versaler: {}", capitalized_text);
}
```
Output:
```
Original: rust är trevligt!
Med versaler: RUST ÄR TREVLIGT!
```

## Fördjupning
I Rust sker kapitalisering genom metoden `.to_uppercase()` från standardbiblioteket, vilket hanterar unicode-strängar korrekt. Förr i tiden var kapitalisering begränsad till ASCII och gav ofta problem med internationella alfabet. Alternativ till `.to_uppercase()` inkluderar egna funktioner för att kontrollera varje tecken eller att använda bibliotek som `unicase` för case-insensitive jämförelser. För att förvandla första bokstaven i varje ord till versal använder du `.to_title_case()`, även om detta inte är inbyggt i Rust och kan kräva ytterligare bibliotek.

## Se även:
- Rusts standardbiblioteks dokumentation om `.to_uppercase()`: https://doc.rust-lang.org/std/primitive.str.html#method.to_uppercase
- Rustdoc på `unicase`-krate för Unicode och ASCII case-folding: https://docs.rs/unicase
- Diskussion och exempel på Rust forum om att hantera textomvandlingar: https://users.rust-lang.org
