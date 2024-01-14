---
title:    "Swift: Lesing av kommandolinjeargumenter"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Hvorfor

Å lese kommandolinje-argumenter er en viktig ferdighet for alle Swift-utviklere. Det lar deg gi dynamisk input til programmene dine, åpne døren for avanserte interaksjoner og gjør kodingen din mer fleksibel og tilpasningsdyktig. Så uansett om du nettopp har begynt med Swift eller har vært en erfaren utvikler i årevis, er det å kunne lese kommandolinje-argumenter en nøkkel til å bli en vellykket Swift-programmerer.

## Hvordan

For å lese kommandolinje-argumenter i Swift, må du først få tilgang til objektet CommandLine. Dette gjøres ved å importere Foundation og deretter ved å bruke CommandLine.arguments. Dette vil gi deg en matrise med alle kommandolinje-argumentene som ble angitt ved å kjøre programmet ditt.

La oss si at du ønsker å lese en enkelt argument og skrive ut det som er angitt. For å gjøre dette, kan du bruke følgende kode:

```Swift
import Foundation

let arguments = CommandLine.arguments
if arguments.count > 1 {
    print("Det første argumentet som er angitt er \(arguments[1])")
}
```

Ved å kjøre programmet ditt med et argument, for eksempel `./programName swift`, vil output være:

`Det første argumentet som er angitt er swift`

For å lese alle argumentene og skrive dem ut som en liste, kan du bruke følgende kode:

```Swift
import Foundation

let arguments = CommandLine.arguments
if arguments.count > 1 {
    print("Alle argumentene som er angitt er:")
    for argument in arguments {
        print(argument)
    }
}
```

Ved å kjøre programmet ditt med flere argumenter, for eksempel `./programName swift programming language`, vil output være:

```
Alle argumentene som er angitt er:
swift
programming
language
```

## Dykk dypere

Nå som du vet hvordan du kan lese kommandolinje-argumenter i Swift, kan du dykke dypere for å utforske flere muligheter. Du kan bruke denne teknikken til å lage et mer interaktivt program, der brukeren kan angi argumenter for å utføre ulike oppgaver. Du kan også bruke argumentene til å totyde eller endre noen innebygde funksjoner i programmet ditt basert på hva brukeren ønsker.

Det er også viktig å merke seg at CommandLine-objektet har andre egenskaper og metoder som kan være nyttige når du jobber med kommandolinje-argumenter. Utforsk dokumentasjonen for å lære mer om mulighetene.

## Se også

- The Swift Programming Language: https://docs.swift.org/swift-book/index.html
- CommandLine-objektet i Foundation-dokumentasjonen: https://developer.apple.com/documentation/foundation/commandline