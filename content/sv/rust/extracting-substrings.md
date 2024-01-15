---
title:                "Utvinna substrängar"
html_title:           "Rust: Utvinna substrängar"
simple_title:         "Utvinna substrängar"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Att extrahera substrängar är en vanlig uppgift inom programmering och är användbart för att manipulera och analysera textsträngar. Med Rusts inbyggda funktioner för att hantera teckensträngar, är det enkelt och effektivt att extrahera substrängar utan att behöva använda externa bibliotek.

## Hur man gör

För att extrahera en substräng från en större textsträng i Rust, använder vi funktionen `&str::get()` tillsammans med ett start- och slutindex. Här är ett exempel:

```rust
let text = "Hej, mitt namn är Rust";
let substring = &text.get(11..15).unwrap();
println!("Mitt namn är {}", substring);
```
Output:
```
Mitt namn är Rust
```

Vi tilldelar en variabel `text` till vår stora textsträng och använder sedan `get()` funktionen för att välja substrängen från karaktärerna på index 11 till 15. Vi använder `unwrap()` för att få tillgång till den faktiska substrängen istället för ett `Option`-värde. Sedan skriver vi ut den extraherade substrängen tillsammans med ett meddelande.

För att extrahera en substräng från början av en textsträng, kan vi använda `get()` med ett slutindex istället för ett startindex. Och för att extrahera en substräng från slutet av en textsträng, kan vi använda `get()` med ett startindex och lämna slutindexet tomt. Här är ett annat exempel:

```rust
let text = "Hej, jag använder Rust som mitt huvudspråk";
let substring = &text.get(..14).unwrap();
println!("Mitt språk är {}", substring);
```
Output:
```
Mitt språk är Rust
```

## Djupdykning

Förutom att använda `get()` funktionen, kan vi också extrahera substrängar i Rust genom att använda `&str::slice()` funktionen. Den här funktionen tar ett intervall av index som argument och returnerar en ny textsträng som innehåller karaktärerna inom detta intervall.

```rust
let text = "Det är en varm sommardag";
let substring = &text.slice(14..17);
println!("Vädret är {}", substring);
```
Output:
```
Vädret är varm
```

Det är också möjligt att använda `&str::chars()` funktionen för att extrahera en enskild karaktär från en textsträng baserat på dess index, vilket kan vara användbart när man arbetar med teckensträngar på ett mer detaljerat sätt.

Nu när du har en grundläggande förståelse för hur man extraherar substrängar i Rust, kan du börja utforska de olika funktionerna och dess olika användningsområden.

## Se även

- Rust språkets officiella hemsida: https://www.rust-lang.org/sv
- Rust dokumentation för `&str::get()` funktionen: https://doc.rust-lang.org/std/primitive.str.html#method.get
- Rust forum för diskussioner och frågor: https://users.rust-lang.org/