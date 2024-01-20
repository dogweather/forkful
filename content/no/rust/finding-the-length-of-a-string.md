---
title:                "Finne lengden på en streng"
html_title:           "Arduino: Finne lengden på en streng"
simple_title:         "Finne lengden på en streng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å finne lengden på en streng betyr rett og slett å telle antall tegn i den. Dette gjør vi for å kontrollere dataene våre, som å sørge for at brukerinput ikke overskrider et gitt antall tegn.

## Slik gjør du:
Her er et enkelt eksempel i Rust på hvordan du kan finne lengden på en streng:

```Rust
fn main() {
    let tekst = "Hei, Norge!";
    let lengde = tekst.len();
    println!("Lengden på teksten er {} tegn.", lengde);
}
```

Kjører du denne koden, vil du se følgende utskrift:

```Rust
Lengden på teksten er 11 tegn.
```

## Dypere inn
Historisk sett har programmeringsspråk hatt forskjellige metoder for å håndtere strenger og deres lengder. Noen språk, som C, bruker terminerende null-bytes, mens andre, som Python og Rust, lagrer lengden som en del av strengobjektet. 

Et alternativ til `len()` metoden er å bruke en løkke for å telle tegnene manuelt, men dette er langsomt og unødvendig komplekst. 

Rust bruker utf-8 tegnkoding, som betyr at hvert tegn kan være 1 til 4 bytes lange. Methoden `len()` returnerer derimot antall bytes, ikke antall tegn. For å få antall tegn, bruker vi `chars().count()`:

```Rust
fn main() {
    let tekst = "Hei, Norge!";
    let tegn = tekst.chars().count();
    println!("Antall tegn i tekst er {}.", tegn);
}
```

## Se også
Rust dokumentasjon i forbindelse med Strings:
- [Rust String-dokumentasjon](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rust len() metode](https://doc.rust-lang.org/std/string/struct.String.html#method.len)
- [Rust chars() metode](https://doc.rust-lang.org/std/string/struct.String.html#method.chars)