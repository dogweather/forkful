---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

**Hva er sletting av tegn som matcher et mønster?** Det er prosessen med å finne og fjerne bestemte tegn fra en string basert på et spesifikt mønster. **Hvorfor gjør programmerere dette?** Jo, det hjelper oss med å manipulere og rense data mer effektivt.

## Hvordan:

La oss se på et eksempel i Rust. Her prøver vi å slette alle tallene i en tekststreng.

```Rust
let s = "He1llo2 Wo3rld4!";
let chars_to_remove: &[char] = &['1', '2', '3', '4'];

let cleaned: String = s.chars().filter(|c| !chars_to_remove.contains(c)).collect();
println!("{}", cleaned);
```

Når du kjører denne koden, vil outputen bli:

```Rust
Hello World!
```
## Dyp Dykk

Når det gjelder **historisk kontekst**, startet dette med tidlige programmeringsspråk, men det har fått ny betydning med moderne databehandling. Å fjerne uønskede tegn matcher gjør det enklere å bearbeide data.

For **alternativer**, vurder bruk av regulære uttrykk som kan gi mer fleksibilitet, men kan være mer kompleks. For eksempel, i Rust:

```Rust
use regex::Regex;
let re = Regex::new(r"\d").unwrap();
let s = "He1llo2 Wo3rld4!";
let result = re.replace_all(&s, "");
println!("{}", result);
```
Vedrørende **implementeringsdetaljer**: metoden vi brukte i 'Hvordan:' seksjonen, bruker Rusts `chars()'-metode for å iterere over hvert tegn i strengen, `filter()` for å fjerne de uønskede tegnene, og `collect()` til slutt for å bygge den nye strengen uten de uønskede tegnene.

## Se Også

- [Rust Programmeringsspråk](https://www.rust-lang.org/)
- [Regex Crate](https://crates.io/crates/regex)
- [Rust Standard Library](https://doc.rust-lang.org/stable/std/)
- [Arbeider med tekster i Rust](https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-string-manipulations.html)