---
title:    "Rust: Kombinering av strenger"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Hvorfor

I denne bloggposten vil vi ta en titt på hvordan du kan slå sammen strenger i Rust. Dette er nyttig når du ønsker å kombinere tekst fra ulike kilder eller bygge komplekse strenger for utskrift eller til bruk i programmer.

## Hvordan gjøre det

For å slå sammen strenger i Rust, kan du bruke operatøren `+` eller `format!` makroen. La oss se på et eksempel:

```Rust
fn main() {
    let navn = "Ole";
    let alder = 30;
    
    let setning = navn + " er " + &alder.to_string() + " år gammel.";
    println!("{}!", setning);
}
```

Dette vil gi følgende utskrift:

```
Ole er 30 år gammel.
```

Her bruker vi `+` operatøren til å kombinere strenger, men vi må konvertere alderen til en streng ved å bruke `.to_string()` metoden. Legg også merke til at vi bruker `&` for å få en referanse til alder-variabelen når vi slår den sammen med strengen.

For et enklere alternativ kan du bruke `format!` makroen slik:

```Rust
fn main() {
    let navn = "Kari";
    let alder = 25;
    
    let setning = format!("{} er {} år gammel.", navn, alder);
    println!("{}!", setning);
}
```

Dette vil også gi samme utskrift som ovenfor.

## Dypdykk

Når du slår sammen strenger ved hjelp av `+` operatøren, vil Rust automatisk konvertere til `String` type. Dette betyr at det blir gjort ekstra arbeid under panseret for å kombinere strenger. Derfor kan det være mer effektivt å bruke `format!` makroen, spesielt hvis du har mange strenger som skal slås sammen.

En annen viktig ting å merke seg er at når du bruker `+` operatøren, vil den forbruke de originale strengene som brukes i uttrykket. Dette betyr at de ikke kan brukes igjen senere i koden. Derfor er det viktig å bruke `format!` makroen eller `.to_string()` metoden hvis du trenger å beholde de originale strengene.

## Se også

- [Rust dokumentasjon om slå sammen strenger](https://doc.rust-lang.org/std/string/struct.String.html#impl-Add%3C%26%27_%20str%3E)
- [Rust makroer](https://doc.rust-lang.org/rust-by-example/metaprogramming/macros.html)