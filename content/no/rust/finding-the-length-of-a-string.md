---
title:                "Rust: Finner lengden på en streng"
simple_title:         "Finner lengden på en streng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Det å finne lengden til en string er en vanlig oppgave i programmering og kan være nyttig i mange sammenhenger. Det kan hjelpe deg med å formatere teksten din riktig, begrense input fra brukere, og generelt sett gjøre koden din mer effektiv.

## Slik gjør du det
For å finne lengden til en string i Rust, kan du bruke funksjonen `.len()`. Denne funksjonen returnerer lengden til stringen som et heltall. La oss se på et eksempel:

```Rust
let string = "Hei, verden!";
println!("Lengden til stringen er {}", string.len());
```

Output vil bli “Lengden til stringen er 13”, siden det er 13 tegn i stringen “Hei, verden!”.

En viktig ting å huske på er at `.len()` returnerer lengden i bytes, ikke i antall tegn. Dette betyr at visse unicode-tegn kan ta opp mer enn én byte, og derfor vil de telle som flere tegn i lengden. Det er viktig å være klar over dette når du arbeider med strings som inneholder unicode-tegn.

Du kan også bruke `.chars().count()` for å få lengden i antall tegn istedenfor bytes. Dette vil telle antall tegn, uavhengig av antall bytes de tar opp. Her er et eksempel:

```Rust
let string = "Hei, verden!";
println!("Antall tegn i stringen er {}", string.chars().count());
```

Output vil bli “Antall tegn i stringen er 11”, siden det er 11 tegn i stringen “Hei, verden!”.

## Dykk dypere
Nå som du vet hvordan du finner lengden til en string, kan det være lurt å forstå hvordan `.len()` funksjonen faktisk fungerer bak kulissene. Det er fordi `.len()` er en del av standardbiblioteket i Rust og er implementert ved hjelp av `str::len()` funksjonen.

`str::len()` funksjonen itererer gjennom alle tegnene i stringen og bruker forholdet mellom byte-lengde og tegn-lengde for å beregne den faktiske lengden til stringen. Dette er enkelt for ASCII-tegn, siden hvert tegn tar opp én byte, men for unicode-tegn som tar opp mer enn en byte, må funksjonen telle antall tegn basert på deres byte-antall.

## Se også
- [Rust Strings dokumentasjon](https://doc.rust-lang.org/std/string/index.html)
- [Rust Standard Library dokumentasjon](https://doc.rust-lang.org/std/index.html)
- [Unicode og UTF-8 i Rust](https://unicode-rs.github.io/unicode-rs/utf8/index.html)