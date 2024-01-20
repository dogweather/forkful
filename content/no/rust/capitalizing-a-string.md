---
title:                "Gjøre en streng stor"
html_title:           "Rust: Gjøre en streng stor"
simple_title:         "Gjøre en streng stor"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Rust Programmering: Hvordan Kapitalisere en Streng

## Hva & Hvorfor?
Å kapitalisere en streng innebærer å gjøre det første bokstavet i strengen stort, mens resten av strengen blir små bokstaver. Programmers bruker det for å standardisere input-data eller øke lesbarheten. 

## Hvordan gjør man dette:
Her er et eksempel på hvordan du kan kapitalisere strenger i Rust:

```Rust
fn main() {
    let my_string = "hello world".to_string();
    let capitalized_string = capitalize(&my_string);

    println!("{}", capitalized_string);
}

fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(first_char) => first_char.to_uppercase().chain(chars).collect(),
    }
}
```
Når du kjører dette programmet, vil output være:

```Rust
Hello world
```

## Dypdykk
Historisk sett har kapitalisering av strenger værem en viktig del av mange programmeringsspråk for å tillate bedre manipulering og representasjon av tekstbasert data.

Alternativt, i Rust, kan du også bruke biblioteket `titlecase` for å kapitalisere strengene dine:

```Rust
use titlecase::titlecase;

fn main() {
    let my_string = "hello world";

    println!("{}", titlecase(my_string));
}
```
Men det er verdt å merke seg at `titlecase` omformer hele strengen til tittelstil, ikke bare det første bokstavet.

## Se også
Hvis du ønsker å fordype deg mer i strengbehandling i Rust, anbefaler vi disse kildene:
- [The Rust Programming Language Book](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Rust by Example: Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)
- [Rust String Methods Documentation](https://doc.rust-lang.org/std/string/struct.String.html)