---
date: 2024-01-20 17:46:54.432830-07:00
description: "Hvordan: I Rust er en streng en rekke av UTF-8 bytes. Det er viktig\
  \ \xE5 huske p\xE5 at \xE5 trekke ut substrings ikke n\xF8dvendigvis handler om\
  \ antall tegn, men\u2026"
lastmod: '2024-04-05T21:53:41.541597-06:00'
model: gpt-4-1106-preview
summary: I Rust er en streng en rekke av UTF-8 bytes.
title: Uthenting av delstrenger
weight: 6
---

## Hvordan:
```Rust
fn main() {
    let text = "Hei, Norge!";
    let start = 5;
    let end = 10;
    
    let substring = &text[start..end];
    
    println!("Utvalgt substring: {}", substring); // Skriver ut: Utvalgt substring: Norge
}
```

Output:

```
Utvalgt substring: Norge
```

## Dypdykk:
I Rust er en streng en rekke av UTF-8 bytes. Det er viktig å huske på at å trekke ut substrings ikke nødvendigvis handler om antall tegn, men bytes.

Hvis vi ser tilbake, ser vi at mange eldre språk brukte enklere ASCII-tegn, hvor ett tegn var lik én byte. Dette gjorde substring-ekstraksjon ganske rett frem. Rust, derimot, støtter UTF-8, som betyr at ett tegn kan være flere bytes.

Det finnes alternativer til å bruke byte-indeks for å lage substrings. For eksempel metoder som `chars()` og `split()` i Rusts standard bibliotek, som håndterer Unicode-tegn korrekt. Men, disse metodene returnerer ofte iterators og ikke strenger direkte, så ekstra steg kan være nødvendige for å få en faktisk substring.

For ytelse og sikkerhet utfører Rust sjekker under kjøring for å sørge for at substring-ekstraktioner ikke ender midt i en gyldig UTF-8-sekvens, noe som kan medføre panikk i programmet. Det er derfor kritisk at programmererne håndterer `Result` eller `Option` typer når de arbeider med substrings, for å unngå å krasje programmet.

## Se Også:
- Rust sin dokumentasjon på strenger: https://doc.rust-lang.org/std/string/index.html
- `slice`-modulen i Rust standard bibliotek: https://doc.rust-lang.org/std/slice/
- En guide på Unicode og strenger i Rust: https://doc.rust-lang.org/book/ch08-02-strings.html
