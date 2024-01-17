---
title:                "Sammenslåing av strenger"
html_title:           "Rust: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sammenføyning av strenger, også kjent som "string concatenation" på engelsk, er en vanlig praksis blant programmerere. Dette innebærer å kombinere to eller flere strenger til en enkelt streng. Dette kan være nyttig for å skape mer dynamiske og tilpassede utdata i programmering.

## Hvordan:
I Rust kan vi enkelt sammenføye strenger ved hjelp av operatoren `+`. La oss se på et eksempel:

```Rust
let navn = "Elin";
let yrke = "utvikler";
let setning = navn + " er en " + yrke;
println!("{}", setning);
```

Dette vil gi oss følgende utput:

```Rust
Elin er en utvikler
```

Vi kan også bruke `&` operatoren for å sammenføye strenger på en mer effektiv måte. La oss se på et annet eksempel:

```Rust
let bilmerke = "Tesla";
let årsmodell = 2021;
let setning = format!("Jeg kjøpte en {} i {}", bilmerke, årsmodell);
println!("{}", setning);
```

Dette vil gi oss følgende utput:

```Rust
Jeg kjøpte en Tesla i 2021.
```

## Dypdykk:
Siden strenger er uforanderlige i Rust, vil det å sammenføye flere strenger resultere i å lage en ny streng hver gang. Dette kan være ineffektivt og føre til ytelsesproblemer. Alternativt kan vi bruke `String` typen, som kan endres og sammenføyes mer effektivt. Dette kan være spesielt nyttig for større programmer som må håndtere mange strenger.

## Se også:
- [String concatenation in Rust](https://rust-lang-nursery.github.io/rust-cookbook/data/string.html#concatenation)
- [Rust Strings](https://doc.rust-lang.org/std/string/index.html)