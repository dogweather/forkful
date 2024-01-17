---
title:                "Konvertere en streng til små bokstaver."
html_title:           "Rust: Konvertere en streng til små bokstaver."
simple_title:         "Konvertere en streng til små bokstaver."
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

"Konvertering av en streng til små bokstaver" betyr å endre alle bokstavene i en tekststreng til små, ikke-store, bokstaver. Dette er nyttig for å sikre ensartethet og konsistens i dataene som behandles i et program. Mange programmerere gjør dette for å unngå feil og uønskede resultater når de sammenligner og manipulerer tekststrenger.

## Hvordan:

```Rust
fn main() {
     let streng = "NoRgEsEaL".to_lowercase();
     println!("{}", streng);
}

# Output:
norseseal 
```

```Rust
fn main() {
    let stor_streng = String::from("Programmering er gøy");
    let liten_streng = stor_streng.to_lowercase();
    println!("{}", liten_streng);
}

# Output:
programmering er gøy
```

## Dykk dypere:

Konvertering av tekststrenger til små bokstaver har vært en del av programmeringsspråk siden tidlig på 1900-tallet, da ASCII-standarden ble opprettet for å standardisere hvordan tastaturopplysninger skulle representeres slik at datamaskiner kunne forstå det. I tillegg til å bruke funksjonen "to_lowercase", kan programmerere også bruke metoder som “str.to_ascii_lowercase” for å utføre denne oppgaven.

En alternativ tilnærming til å konvertere tekststrenger er å bruke "upper" og "lower" funksjoner til å forandre bokstaver individuelt. Dette kan være nyttig når man trenger å håndtere spesielle tegn som ikke følger ASCII-standarden.

I Rust implementeres konvertering av tekststrenger til små bokstaver ved hjelp av Unicode-standardbiblioteker, som tillater håndtering av alle bokstavene i verden. Dette sikrer at programmer skrevet på Rust er mer fleksible og tilpasningsdyktige når det gjelder håndtering av forskjellige språk og skriftilpassinger.

## Se også:

https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase