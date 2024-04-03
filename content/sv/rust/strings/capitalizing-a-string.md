---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:36.443516-07:00
description: "Att g\xF6ra f\xF6rsta bokstaven i en str\xE4ng stor i Rust inneb\xE4\
  r att modifiera str\xE4ngen s\xE5 att dess f\xF6rsta tecken blir en stor bokstav\
  \ om det \xE4r en bokstav,\u2026"
lastmod: '2024-03-13T22:44:37.681306-06:00'
model: gpt-4-0125-preview
summary: "Att g\xF6ra f\xF6rsta bokstaven i en str\xE4ng stor i Rust inneb\xE4r att\
  \ modifiera str\xE4ngen s\xE5 att dess f\xF6rsta tecken blir en stor bokstav om\
  \ det \xE4r en bokstav, medan resten av str\xE4ngen l\xE4mnas of\xF6r\xE4ndrad."
title: "G\xF6r om en str\xE4ng till versaler"
weight: 2
---

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
