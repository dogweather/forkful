---
title:    "Rust: Å bruke regulære uttrykk"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor bør du bruke regulære uttrykk i din Rust programmering? Regulære uttrykk er en kraftig måte å søke og manipulere tekst på. Det kan være nyttig for å finne spesifikke mønstre i en tekst, validere brukerinput eller erstatte deler av en streng. Det er også et viktig verktøy i mange programmeringsspråk, så å lære å bruke det i Rust vil være en verdifull ferdighet å ha.

## Hvordan
For å bruke regulære uttrykk i Rust, trenger du først å importere Regex biblioteket. Deretter kan du definere et uttrykk ved hjelp av Regex::new() funksjonen og deretter bruke metoder som .is_match() for å se om strenger passer til uttrykket, eller .replace() for å erstatte deler av en streng.

La oss si at du vil finne alle tall i en tekst og erstatte dem med "NUMMER". Dette kan enkelt gjøres ved hjelp av følgende kode:

```Rust 
use regex::Regex;

let re = Regex::new(r"[0-9]+").unwrap();
let text = "Dette er en setning med tall 123 og 456";

let result = re.replace_all(text, "NUMMER");

println!("{}", result); // Dette er en setning med tall NUMMER og NUMMER
```

Som du kan se, tar Regex::new() funksjonen inn et mønster (her [0-9]+ som betyr en eller flere tall) og deretter bruker vi .replace_all() metoden for å erstatte alle forekomster av mønsteret med "NUMMER".

## Dypdykk
Regulære uttrykk kan være komplekse, men å lære å bruke dem vil gjøre det enkelt å utføre avansert tekstbehandling. Det finnes mange forskjellige symbolske uttrykk og metoder for å gjøre søk og erstatninger enda mer presise. Det er også mulig å lage dynamiske uttrykk ved hjelp av variabler og løkker.

For å få en dypere forståelse av regulære uttrykk og hvordan du kan bruke det i din Rust programmering, anbefaler vi å se på offisiell dokumentasjon for Regex biblioteket og prøve ut ulike eksempler.

## Se også
- [Offisiell dokumentasjon for Regex biblioteket i Rust](https://docs.rs/regex/1.4.2/regex/)
- [Tutorial for å lære regulære uttrykk i Rust](https://medium.com/@ericdreichert/regex-in-rust-a06fc30ad58e)
- [En interaktiv guide til regulære uttrykk i Rust](https://fosskers.github.io/writing-good-initialization.html#sec-6)
- [Eksempler på bruk av regulære uttrykk i praktiske situasjoner](https://dev.to/dev0928/rust-regex-penetrating-examination-and-practice-3io2)