---
title:                "Sammenføyning av strenger"
html_title:           "Rust: Sammenføyning av strenger"
simple_title:         "Sammenføyning av strenger"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere strenger, eller concat strings på engelsk, er en viktig del av programmering. Det lar deg lage nye, lengre setninger eller variabler ved å kombinere eksisterende strenger.

## Hvordan

```Rust
fn main() {
  let navn = "Anna";
  let yrke = "programmerer";
  let setning = format!("Hei, jeg heter {} og jeg er en {}!", navn, yrke);
  println!("{}", setning);
}
```

```Rust
Output:
Hei, jeg heter Anna og jeg er en programmerer!
```

For å kombinere strenger i Rust, bruker vi `format!` makroen. Den tar inn en "template" string, der variabler og termer kan plasseres ved hjelp av `{}`. Disse variablene blir deretter definert utenfor makroen og blir satt inn i template-strengen i samme rekkefølge.

## Dykker dypere

Rust har også `println!` og `print!` makroer som fungerer på samme måte som `format!`, men i stedet for å returnere en ny string, blir den skrevet ut til konsollen. Det er også mulig å bruke `+` -operatøren for å kombinere to strenger, men denne metoden er mindre effektiv og anbefales ikke for store mengder data.

En viktig ting å merke seg er at Rust er enten-byttbart når det gjelder å kombinere strenger. Dette betyr at du kan kombinere en snei med en variabel av typen "string" uten å måtte konvertere den første til en streng først.

## Se også

- [Offisiell Rust dokumentasjon om strenger](https://doc.rust-lang.org/std/string/)
- [Rust By Example: Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)
- [Rustlings: Strings](https://github.com/rust-lang/rustlings/blob/main/exercises/strings/strings1.rs)