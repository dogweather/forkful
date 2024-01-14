---
title:                "Rust: Skrivande till standardfel"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standardfelutgången (standard error) är en viktig del av felsökning och utveckling i Rust-programmering. Genom att skriva till standard error, kan du få en mer detaljerad och specifik förståelse av dina program, vilket kan hjälpa dig att hitta och åtgärda fel snabbare.

## Så här gör du

För att skriva till standard error i Rust, kan du använda funktionen `eprintln!()` som skriver till standardfelutgången med en liknande syntax som `println!()`. Till exempel:

```
fn main() {
    let error_message = "Ett fel har uppstått!";
    eprintln!("{}", error_message);
}
```

Det här kommer att skriva "Ett fel har uppstått!" direkt till standardfelutgången.

För att se resultatet av detta kodexempel, kan du köra programmet med kommandot `cargo run` i din terminal, och du kommer att få följande output:

```
Ett fel har uppstått!
```

## Djupdykning

Det finns flera fördelar med att skriva till standard error:

- Det ger dig möjlighet att skriva mer detaljerade och specifika felmeddelanden, vilket kan hjälpa dig att snabbare identifiera och lösa problem med din kod.
- Genom att skriva till standard error, kan du skilja mellan standardutgången (standard output) och standardfelutgången, vilket gör det enklare att förstå vilka delar av din kod som genererar output.
- I kombination med loggningsbibliotek i Rust, kan skrivning till standard error vara en viktig del av felsökning och spårning av problem i produktion.

Det är också viktigt att notera att standard error bara används för att skriva ut felmeddelanden och inte för annan typ av output. För all annan output ska du använda `println!()`.

## Se även

- [Rust dokumentation - Standard input, output och felutgång](https://doc.rust-lang.org/std/io/index.html)
- [YouTube videor om felsökning och loggning i Rust](https://www.youtube.com/results?search_query=rust+error+logging)
- [Rust Cookbook - Att skriva till standard error](https://rust-lang-nursery.github.io/rust-cookbook/development_tools/debugging/print_debug_messages.html#write-to-standard-error)