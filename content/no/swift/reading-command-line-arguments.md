---
title:                "Swift: Å lese kommandolinje-argumenter"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hvorfor
Hvorfor lese kommandolinjeargumenter? Det kan være nyttig når du ønsker å gi input til et program ved å bruke kommandolinjen istedenfor å endre koden manuelt.

# Hvordan
For å lese kommandolinjeargumenter i Swift, kan du bruke følgende kode:
```Swift
let args = CommandLine.arguments
// args er en array med alle kommandolinjeargumentene
```

Hvis du ønsker å ta ut et spesifikt argument, kan du bruke indeksering:
```Swift
let firstArg = args[0]
// Dette vil gi deg det første argumentet som ble gitt ved kjøring av programmet
```

Du kan også sjekke antall argumenter ved hjelp av `args.count` og bruke en for-løkke for å behandle alle argumentene.

# Deep Dive
Hvis du vil ha mer inngående informasjon om kommandolinjeargumenter, kan du utforske `CommandLine`-strukturen i Swift. Denne strukturen gir deg blant annet tilgang til programnavnet (`CommandLine.arguments[0]`), muligheten til å legge til egne argumenter og å lese miljøvariabler.

# Se Også 
- [Swift offisiell dokumentasjon om CommandLine](https://developer.apple.com/documentation/foundation/commandline)
- [Tutorial om lesing av kommandolinjeargumenter i Swift](https://www.educative.io/edpresso/how-to-read-command-line-arguments-in-swift)