---
title:    "Gleam: Lesing av kommandolinje-argumenter"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor lese kommandolinjeargumenter i Gleam? Kommandolinjeargumenter er en viktig del av mange programmer og lar brukeren påvirke hvordan programmet kjører. Ved å forstå hvordan man leser og bruker disse argumentene i Gleam, kan du lage mer fleksible og tilpasningsdyktige programmer.

## Hvordan

For å lese kommandolinjeargumenter i Gleam, kan du bruke funksjonen `Arguments.all`. Denne funksjonen tar ingen argumenter og returnerer en liste med strenger som representerer de ulike argumentene som ble gitt ved kjøring av programmet.

```Gleam
import gleam/arguments

pub fn main() {
  args = arguments.all()
  for arg in args {
    io.println(arg)
  }
}
```

Hvis du for eksempel kjører programmet ditt med følgende kommandolinje:

```
gleam run program.gleam arg1 arg2 arg3
```

Vil følgende bli skrevet ut:

```
program.gleam
arg1
arg2
arg3
```

Den første strengen representerer navnet på programmet, mens de påfølgende strengene representerer de ulike argumentene som ble gitt.

## Dypdykk

Det er også mulig å bruke funksjonen `arguments.flag` for å få tak i spesifikke flagg som ble gitt som kommandolinjeargumenter. Denne funksjonen tar en streng som parameter og returnerer en boolesk verdi som indikerer om flagget ble gitt eller ikke.

```Gleam
import gleam/arguments

pub fn main() {
  verbose = arguments.flag("v")

  if verbose {
    io.println("Programmet kjører i verbose modus.")
  }
}
```

Hvis du kjører programmet ditt med kommandolinjen `gleam run program.gleam -v`, vil "Programmet kjører i verbose modus." bli skrevet ut.

## Se også

- [Gleam dokumentasjon](https://gleam.run)
- [Gleam kommandolinjeargumenter](https://gleam.run/articles/command-line-arguments)
- [Gleam standardbibliotek](https://github.com/gleam-lang/gleam_stdlib)