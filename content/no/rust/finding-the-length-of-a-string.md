---
title:                "Finn lengden av en streng"
html_title:           "Rust: Finn lengden av en streng"
simple_title:         "Finn lengden av en streng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Lurer du på hvordan du kan finne lengden til en tekststreng i Rust? Vel, det er faktisk ganske enkelt! Å kunne finne lengden til en tekststreng er en nyttig ferdighet som kan hjelpe deg med å behandle og manipulere tekst på en mer effektiv måte.

## Slik gjør du det

For å finne lengden til en tekststreng i Rust, kan du bruke den innebygde funksjonen `len()`. La oss se på et eksempel:

```
fn main() {
  let tekststreng = "Hei alle sammen!";
  let lengde = tekststreng.len();
  println!("Lengden til tekststrengen er {}", lengde);
}
```

Dette koden vil produsere følgende utdata i terminalen:

```
Lengden til tekststrengen er 17
```

I dette eksempelet definerte vi en tekststreng, brukte `len()` funksjonen for å finne lengden til strengen og deretter skrev ut lengden til terminalen.

## Dykk dypere

Det er viktig å merke seg at `len()` funksjonen faktisk returnerer antall bytes som brukes til å representere tekststrengen, og ikke antall tegn. Dette kan føre til forskjellige resultater for unicode-tegn. Hvis du vil finne antall tegn i en unicode-tekststreng, kan du bruke `chars()` funksjonen i stedet.

I tillegg har Rust også `is_empty()` funksjonen som lar deg sjekke om en tekststreng er tom eller ikke. Du kan bruke denne funksjonen sammen med `len()` for å håndtere tomme tekststrenger i dine programmer.

## Se også

- [Rust dokumentasjon for `len()` funksjonen](https://doc.rust-lang.org/std/primitive.str.html#method.len)
- [Mer informasjon om å jobbe med tekststrenger i Rust](https://doc.rust-lang.org/book/ch08-02-strings.html)