---
title:                "Sammenslåing av strenger"
html_title:           "Arduino: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

# Konkatenering av strenger i Rust

## Hva & Hvorfor?
**Konkatenering av strenger** er prosessen med å slå sammen to eller flere strenger til én. Det brukes ofte for å lage dynamiske meldinger og håndtere brukerinput mer effektivt.

## Hvordan:
I Rust kan du konkatenerer strenger ved hjelp av `+` operatøren eller `format!` makroen. Her er noen eksempler:

```Rust
let hello = String::from("Hei, ");
let world = String::from("verden!");
let hello_world = hello + &world; 
println!("{}", hello_world); // Output: "Hei, verden!"
```

Bruk av `format!` makroen:

```Rust
let hello = String::from("Hei, ");
let world = String::from("verden!");
let hello_world = format!("{}{}", hello, world);
println!("{}", hello_world); // Output: "Hei, verden!"
```

## Dyp Dykk
Historisk har konkatenering av strenger vært en grunnleggende del av programmering. Rust har valgt en litt annerledes tilnærming sammenlignet med andre språk. I stedet for å tillate direkte konkatenering med `+` operatøren mellom to strenger, krever Rust at det første uttrykket er en streng og det andre en referanse til en streng.

Alternativt kan `format!` makroen brukes for en mer lesbar konkatenering, spesielt med flere strenger. Denne makroen returnerer en ny streng og unngår feller som kan oppstå med `+` operatøren.

Når det gjelder implementeringsdetaljer, fører konkatenering med `+` til at Rust flytter eierskapet til den nye strengen. Dette betyr at den første strengen ikke kan brukes igjen etter operasjonen. Derimot, når vi bruker `format!`, beholdes de opprinnelige strengene og en ny streng blir returnert.

## Se Også
For mer informasjon om konkatenering av strenger i Rust, ta en titt på følgende ressurser:

- [The Rust Programming Language - The Book](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/std/str.html)
- [StackOverflow - How to concatenate strings](https://stackoverflow.com/questions/30154541/rust-how-to-concatenate-strings)