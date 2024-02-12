---
title:                "Gör om en sträng till versaler"
aliases:
- /sv/rust/capitalizing-a-string.md
date:                  2024-02-03T19:06:36.443516-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gör om en sträng till versaler"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & varför?

Att göra första bokstaven i en sträng stor i Rust innebär att modifiera strängen så att dess första tecken blir en stor bokstav om det är en bokstav, medan resten av strängen lämnas oförändrad. Programmerare utför ofta denna operation för formateringsändamål, som att förbereda ord för titlar eller säkerställa konsekvens i användarinmatning.

## Hur man gör:

För att göra första bokstaven i en sträng stor i Rust har du två huvudvägar: använda standardbibliotekets funktionaliteter eller använda tredjepartskrater för mer komplexa eller specifika behov. Så här kan du göra båda.

### Använda Rusts standardbibliotek

Rusts standardbibliotek erbjuder inte en direkt metod för att göra strängar stora, men du kan åstadkomma detta genom att manipulera strängens tecken.

```rust
fn capitalize_first(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn main() {
    let my_string = "hello";
    println!("{}", capitalize_first(my_string)); // Utmatning: Hello
}
```

### Använda `heck`-lådan

För ett mer rakt på sak tillvägagångssätt, speciellt när man arbetar inom en större textbearbetningskontext, kan du föredra att använda tredjepartsbibliotek som `heck`. `heck`-lådan erbjuder olika funktionaliteter för omvandling av fall, inklusive ett enkelt sätt att göra strängar stora.

Först, lägg till `heck` i din `Cargo.toml`:

```toml
[dependencies]
heck = "0.4.0"
```

Använd sedan den för att göra din sträng stor:

```rust
extern crate heck; // Behövs inte i Rust 2018 utgåvan eller senare
use heck::TitleCase;

fn main() {
    let my_string = "hello world";
    let capitalized = my_string.to_title_case();
    println!("{}", capitalized); // Utmatning: Hello World
}
```

Notera: Metoden `to_title_case` som tillhandahålls av `heck` gör varje ord i strängen stort, vilket kan vara mer än vad du letar efter om du endast vill ha den första bokstaven i strängen stor. Justera din användning enligt dina specifika behov.
