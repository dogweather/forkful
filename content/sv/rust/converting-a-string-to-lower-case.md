---
title:                "Omvandla en sträng till gemener"
html_title:           "Rust: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför
Du kanske undrar varför någon skulle behöva omvandla en sträng till små bokstäver i Rust. Det finns många goda skäl till varför detta är en vanlig uppgift i många programmeringsprojekt. Det kan vara för att göra jämförelser mellan strängar mer exakta, för att skapa en standardiserad form av data eller för att enkelt hantera data som skickas mellan olika system som kanske inte hanterar stora och små bokstäver på samma sätt.

## Så här gör du
För att omvandla en sträng till små bokstäver i Rust använder vi funktionen `to_lowercase()`. Detta är en del av standardbiblioteket `std::string`, så vi behöver inte importera någonting extra för att använda den. Här är ett exempel på hur vi kan använda funktionen:

```Rust
let my_string = "HeLlO wOrLd";
let lower_case_string = my_string.to_lowercase();
println!("{}", lower_case_string);
```
Output:
```Rust
hello world
```

## Djupdykning
För att förstå hur `to_lowercase()` fungerar i bakgrunden, behöver vi titta på Unicode-standarderna. I Unicode finns det två olika typer av bokstäver som kan representeras: stora och små. När vi använder `to_lowercase()` i Rust, så används Unicode-tabellen för att hitta motsvarande små bokstav för varje stor bokstav i strängen. Denna process skiljer sig åt beroende på vilket språk strängen är skriven på, eftersom olika språk kan ha olika regler för omvandling av bokstäver.

## Se även
- [The Rust Standard Library](https://doc.rust-lang.org/std/index.html)
- [Unicode Standard](https://www.unicode.org/standard/standard.html)