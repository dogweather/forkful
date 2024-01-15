---
title:                "Användning av reguljära uttryck"
html_title:           "Rust: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Reguljära uttryck är ett kraftfullt verktyg inom programmering som hjälper dig att söka, matcha och manipulera textsträngar på ett mer effektivt sätt. Det är särskilt användbart när du behöver hantera stora mängder data eller när du arbetar med textbaserade filer.

## Så här använder du reguljära uttryck i Rust

För att använda reguljära uttryck i Rust behöver du importera "regex" biblioteket och skapa en instans av regex-objektet. Här är ett enkelt exempel på hur du kan matcha ett mönster i en textsträng:

```Rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"hej"); // Skapar en regex-instans som matchar "hej"
    let text = "hej alla!";

    if re.is_match(text) { // Kontrollerar om mönstret matchar texten
        println!("Hittade ett matchande mönster.");
    } else {
        println!("Inget matchande mönster hittades.");
    }
}
```

I detta exempel kommer programmet att skriva ut "Hittade ett matchande mönster." eftersom textsträngen innehåller ordet "hej".

Du kan också använda reguljära uttryck för att söka efter specifika mönster och ersätta dem med annan text. Här är ett exempel som byter ut alla siffror i en textsträng med "X":

```Rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\d"); // Skapar en regex-instans som matchar siffror
    let text = "Det finns 123 äpplen i skålen.";

    let result = re.replace_all(text, "X"); // Ersätter alla siffror med "X"
    
    println!("{}", result); // Skriver ut resultatet: "Det finns XXX äpplen i skålen."
}
```

## Djupdykning

Regex-objektet i Rust har många olika metoder som hjälper dig att manipulera textsträngar på olika sätt. Här är några av de vanligaste metoderna:

- *is_match()*: Kontrollerar om ett mönster matchar en textsträng.
- *find()*: Hittar den första matchningen av ett mönster i en textsträng.
- *captures()*: Returnerar alla fångade grupper i en matchning.
- *replace()*: Ersätter en matchning med ny text.
- *split()*: Delar upp en textsträng baserat på ett angivet mönster.

Det finns också många olika metakaraktärer och modifierare som du kan använda för att skapa mer avancerade mönster. För att lära dig mer om dessa, rekommenderar vi dig att läsa dokumentationen för "regex" biblioteket samt experimentera med olika mönster och metoder.

## Se även

- ["regex" bibliotekets dokumentation](https://docs.rs/regex/latest/regex/)
- [En tutorial om reguljära uttryck i Rust](https://www.stoica.dev/regular-expressions-in-rust/)
- [Officiell hemsida för Rust](https://www.rust-lang.org/)