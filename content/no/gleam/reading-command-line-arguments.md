---
title:    "Gleam: Lesing av kommandolinjeargumenter"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang stått fast i en situasjon der du trenger å gi spesifikke instruksjoner til et program fra kommandolinjen? Da kan kunnskap om hvordan du leser kommandolinjeargumenter i Gleam-programmeringsspråket være svært nyttig.

## Hvordan du leser kommandolinjeargumenter i Gleam

For å lese kommandolinjen argumenter i Gleam, kan du bruke biblioteket `std/commandline` som er innebygd i språket. Det første trinnet er å importere biblioteket i koden din:

```Gleam
import std/commandline
```

Deretter kan du bruke funksjonen `Commandline.argv()` for å få en liste over alle argumentene som er gitt i kommandolinjen:

```Gleam
let args = Commandline.argv()
```

For å få en spesifikk argument, kan du bruke indeksering på `args`-listen. For eksempel, hvis du vil få tak i det andre argumentet, kan du gjøre følgende:

```Gleam
let argument = args[1]
```

Du kan også sjekke lengden på argumentlisten og håndtere tilfeller der ingen argumenter er gitt:

```Gleam
let args_length = length(args)
let argument = if args_length > 1 {
  args[1]
} else {
  "Ingen argumenter gitt"
}
```

Nå har du fått tak i kommandolinjeargumentet og du kan bruke det videre i programmet ditt.

## Dypdykk i å lese kommandolinjeargumenter

Det er også andre funksjoner i `std/commandline` biblioteket som kan være nyttige når du leser kommandolinjeargumenter. Du kan for eksempel få tak i navnet på eksisterende flagg ved hjelp av `Commandline.flags()`-funksjonen, og sjekke om et spesifikt flagg er satt ved hjelp av `Commandline.has_flag()`-funksjonen.

Det er også mulig å parse kommandolinjeargumenter som er gitt som strenger ved hjelp av `Commandline.parse()`-funksjonen.

For mer informasjon om hvordan du leser kommandolinjeargumenter i Gleam, kan du lese dokumentasjonen for `std/commandline` biblioteket.

## Se også

- [Dokumentasjon for std/commandline](https://gleam.run/releases/v0.15.0/examples/commandline.html)
- [Gleam programmeringsspråk](https://gleam.run/)