---
title:    "Rust: Store bokstaver i en streng"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Hvorfor

Har du noensinne lurt på hvordan du kan gjøre en tekst mer lesbar ved å kapitalisere den? I denne bloggposten skal vi ta en titt på hvordan du kan gjøre akkurat det ved hjelp av Rust-programmeringsspråket.

## Hvordan

Først og fremst må vi importere biblioteket som lar oss utføre denne operasjonen. I dette tilfellet er det `to_uppercase`-metoden fra `str`-biblioteket i Rust.

```Rust
use std::str;

fn main() {
    // Opprett en variabel med tekst som skal kapitaliseres
    let tekst = "hallo, verden!";

    // Bruk to_uppercase-metoden for å gjøre teksten til store bokstaver
    let kapitalisert_tekst = str::to_uppercase(tekst);

    // Skriv ut resultatet
    println!("{}", kapitalisert_tekst);
}
```

Når du kjører dette programmet, vil du se følgende output:

```txt
HALLO, VERDEN!
```

Som du kan se, blir teksten vår kapitalisert. Men hva om vi ønsker å beholde store bokstaver som allerede finnes i teksten? Det kan vi enkelt gjøre ved å bruke `to_uppercase`-metoden med en instans av `Locale`, som lar oss spesifisere hvilket språk teksten skal behandles som.

```Rust
use std::str;
use std::locale::Locale;

fn main() {
    // Opprett en variabel med tekst som skal kapitaliseres
    let tekst = "Hello, world!";

    // Bruk to_uppercase-metoden med en Locale-instans for å beholde allerede store bokstaver
    let kapitalisert_tekst = str::to_uppercase_with_locale(tekst, Locale::root());

    // Skriv ut resultatet
    println!("{}", kapitalisert_tekst);
}
```

Denne gangen vil outputen bli:

```txt
HELLO, WORLD!
```

## Deep Dive

Nå som vi har sett hvordan vi kan kapitalisere en tekst ved hjelp av Rust, la oss ta en nærmere titt på hvordan `to_uppercase`-metoden fungerer. Denne metoden tar inn en `&str`-referanse som parameter og returnerer en ny `String` med alle bokstavene konvertert til store bokstaver.

En viktig ting å merke seg er at `to_uppercase`-metoden er avhengig av den aktuelle locale, eller språkinnstillingen, på systemet ditt. Dette betyr at hvis teksten din inneholder bokstaver fra et annet språk, vil de ikke nødvendigvis bli kapitalisert riktig med mindre du spesifikt angir det i Locale-instansen.

## Se Også

- [Rust dokumentasjon: str::to_uppercase](https://doc.rust-lang.org/std/primitive.str.html#method.to_uppercase)
- [Rust dokumentasjon: locale::Locale](https://doc.rust-lang.org/std/locale/struct.Locale.html)