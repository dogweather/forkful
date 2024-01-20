---
title:                "Bruk av regulære uttrykk"
html_title:           "Bash: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regular expressions, eller regulære uttrykk, lar deg lete etter mønstre i tekst. Programmerere bruker dette for å finne, validere, manipulere eller trekke ut data på en fleksibel måte.

## Slik gjør du:
For å bruke regulære uttrykk i Rust, installer `regex`-biblioteket. Bruk `Regex::new()` for å kompilere et uttrykk, og søk deretter gjennom tekst med metoder som `is_match`, `find`, eller `captures`.

```Rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\bprogrammer(ing)?\b").unwrap();
    let tekst = "Jeg liker programmering og å være programmerer!";
    
    // Sjekk om det er et treff
    println!("Treff funnet: {}", re.is_match(tekst)); // Treff funnet: true

    // Finn det første treffet
    if let Some(matched) = re.find(tekst) {
        println!("Første treff: {}", &tekst[matched.start()..matched.end()]); // Første treff: programmering
    }
}
```

## Dybdegående
Regulære uttrykk ble popularisert på 1960-tallet av Stephen Kleene. Alternativer til regulære uttrykk inkluderer strengsøk-biblioteker eller innebygde funksjoner som `str::contains` i Rust for enklere mønstre. Rusts `regex`-bibliotek kompilerer regulære uttrykk til bytecode som utføres av en virtual maskin, noe som gir en god balanse mellom ytelse og fleksibilitet.

## Se også:
- Rust `regex` bibliotekets dokumentasjon: https://docs.rs/regex/
- En introduksjonsveileder for regex i Rust: https://doc.rust-lang.org/book/ch18-00-patterns.html
- "The Rust Programming Language" bokens kapittel om mønstre og regulære uttrykk (engelsk): https://doc.rust-lang.org/book/ch18-03-pattern-syntax.html