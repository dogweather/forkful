---
title:                "Å bruke regulære uttrykk"
html_title:           "Rust: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Å bruke regulære uttrykk kan være en effektiv måte å søke og manipulere tekst på i programmering. Det kan være spesielt nyttig når du jobber med data som følger et mønster eller format.

## Hvordan

For å bruke regulære uttrykk i Rust, må vi først importere biblioteket `regex` ved å legge til følgende linje i koden vår:

```rust
extern crate regex;
use regex::Regex;
```

Vi kan deretter opprette et nytt regulært uttrykk ved å passe inn ønsket mønster som en streng til `Regex::new()`-funksjonen:

```rust
let re = Regex::new(r"^\w+").unwrap();
```

I dette tilfellet vil vi ha et uttrykk som søker etter ord som starter med én eller flere bokstaver.

For å teste om et gitt uttrykk matcher med en tekststreng, kan vi bruke `is_match()`-metoden:

```rust
let text = "Dette er en test.";
if re.is_match(text) {
    println!("Første ord i teksten: {}", text.split_whitespace().next().unwrap());
} else {
    println!("Ingen match funnet.");
}
```

I dette tilfellet vil utgangen bli `Første ord i teksten: Dette`.

For å få ut alle matchene i en tekststreng, kan vi bruke `find_iter()`-metoden:

```rust
let text = "Dette er en test av regulære uttrykk.";
for mat ch in re.find_iter(text) {
    println!("Match funnet: {}", mat ch.as_str());
}
```

Dette vil gi oss følgende utgang:

```text
Match funnet: Dette
Match funnet: er
Match funnet: en
```

## Dypdykk

Når vi bruker regulære uttrykk i Rust, kan vi også bruke såkalte "capture groups" for å hente ut spesifikke deler av en matchet tekststreng. For å gjøre dette, plasserer vi bare paranteser rundt delen vi vil hente ut i uttrykket vårt:

```rust
let re = Regex::new(r"^(\w+) (\d+)").unwrap();
let text = "Dette er test 123.";
let mat ch = re.captures(text).unwrap();
println!("Første ord: {}", mat ch.get(1).unwrap().as_str());
println!("Tall: {}", mat ch.get(2).unwrap().as_str());
```

I dette tilfellet vil utgangen være:

```text
Første ord: Dette
Tall: 123
```

Det er også mulig å bruke såkalte "lookahead" og "lookbehind" i uttrykkene våre. Dette lar oss søke etter tekst som er foran eller etter en bestemt del av vår matchede tekst. Mer informasjon om dette finner du i [Rust sin dokumentasjon](https://docs.rs/regex/1.3.1/regex/#lookaround).

## Se også
- [Rust sin offisielle nettside](https://www.rust-lang.org/no)
- [Regulære uttrykk tutorial på Rust-språket](https://chrismorgan.info/blog/rust-regex-tutorial/)
- [Utforske Rust sin regex-bibliotek](https://docs.rs/regex/1.3.1/regex/)