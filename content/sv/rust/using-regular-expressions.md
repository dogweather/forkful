---
title:                "Rust: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Regular expressions är ett kraftfullt verktyg för strängmanipulering och mönstermatchning. Med hjälp av dessa uttryck kan du snabbt och effektivt hitta specifika strängar eller mönster i en textsträng. Detta kan vara särskilt användbart för datafiltrering, dataanalys och webbutveckling.

## Så här gör du

För att använda regular expressions i Rust, måste du först importera biblioteket `regex`. Sedan kan du skapa ett nytt uttryck med hjälp av funktionen `Regex:: new`. Till exempel:
```Rust
use regex::Regex;

let re = Regex::new(r"hej");
```
Här skapar vi ett uttryck som matchar strängen "hej". Notera att vi använder prefixet `r` före strängen för att markera den som en raw string, vilket gör det enklare att hantera specialtecken.

För att söka efter mönster i en textsträng, använder vi funktionen `find`:
```Rust
let text = "Hej, detta är en textsträng.";
let result = re.find(text);

println!("Resultat: {:?}", result);
```
Detta kommer att returnera en `Option` som antingen är `Some(match)` om uttrycket matchades eller `None` om det inte gjorde det. Om det matchade kan vi få åtkomst till matchen genom att använda `.unwrap()` metoden:
```Rust
let matchen = result.unwrap();
println!("Match: {}", matchen.as_str());
```
Detta kommer att skriva ut den matchande delen av textsträngen, vilket i detta fall skulle vara "hej".

## Djupdykning

Utöver att söka efter mönster kan du också använda regular expressions för att ersätta textsträngar med hjälp av `replace_all` funktionen:
```Rust
let ny_text = re.replace_all(text, "hallå");
println!("Ny text: {}", ny_text);
```
Detta skulle byta ut alla förekomster av "hej" i texten med "hallå".

Du kan också använda "capturing groups" för att fånga specifika delar av texten. Till exempel, om vi vill ha åtkomst till den första bokstaven i varje ord i en textsträng, kan vi använda `Regex::new(r"\b(\w)").unwrap()` och sedan iterera över varje match för att få åtkomst till gruppens innehåll.

För mer information och avancerade användningsområden, se dokumentationen för `regex` biblioteket och andra relaterade bibliotek.

## Se även

- Officiell dokumentation för `regex` biblioteket: https://docs.rs/regex/1.4.2/regex/
- En grundläggande handledning för att arbeta med regular expressions i Rust: https://brandonkal.blogspot.com/2019/02/text-parsing-in-rust-using-regular.html