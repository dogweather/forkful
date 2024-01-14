---
title:                "Gleam: Lese kommandolinje argumenter"
simple_title:         "Lese kommandolinje argumenter"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hvorfor du skulle bry deg med å lese kommandolinjeargumenter i programmene dine? Vel, i denne bloggposten skal vi se nærmere på hvorfor dette er et viktig konsept i Gleam-programmering.

## Hvordan

Å lese kommandolinjeargumenter i Gleam er en enkel prosess som gir stor fleksibilitet til programmene dine. La oss ta en titt på et eksempel:

```Gleam
import gleam/io

// Leser inn et kommandolinjeargument og skriver ut det til konsollen
fn main(args) {
  let argument = get_argument(args)
  io.print(argument)
}

// Funksjon som henter ut det første kommandolinjeargumentet
fn get_argument(args) {
  case args {
    [arg | _] -> arg
    _ -> ""
  }
}
```

Når du kjører dette programmet med følgende kommandolinje:

```bash
gleam run my_program.gleam my_argument
```

Vil konsollen skrive ut "my_argument". Her henter vi ut det første argumentet som ble gitt med ved kjøring av programmet, men du kan også endre koden for å lese inn et spesifikt argument basert på posisjon.

## Dypdykk

Nå som du vet hvordan du leser kommandolinjeargumenter i Gleam, la oss se på noen grunner til hvorfor dette kan være nyttig. Først og fremst kan det være en måte å gi foretrukne instillinger eller parametere til programmene dine uten å måtte endre selve koden. Dette gjør det enkelt å tilpasse og gjenbruke samme program til ulike formål.

Et annet scenario kan være når du ønsker å kjøre mange ulike tester eller tjenester med samme kodebase, men med ulike innstillinger. Ved å lese kommandolinjeargumenter kan du enkelt endre disse innstillingene uten å måtte endre koden hver gang.

## Se også

- [Offisiell Gleam dokumentasjon](https://gleam.run/)
- [Gleam kommandolinjeverktøy](https://github.com/gleam-lang/gleam/blob/master/README.md)
- [Bruk av kommandolinjeargumenter i andre programmeringsspråk](https://www.sitepoint.com/bash-command-line-arguments/)