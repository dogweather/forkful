---
title:    "Swift: Å lese kommandolinje-argumenter"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor
Har du noen gang lurt på hvordan du kan lage et Swift-program som kan ta imot input fra kommandolinjen? Å kunne lese kommandolinjeargumenter er en viktig ferdighet i programmering, da det gjør det mulig for brukeren å gi spesifikke instruksjoner til programmet. I denne bloggposten vil vi utforske hvordan du kan lese og behandle kommandolinjeargumenter i Swift.

## Hvordan gjøre det
For å lese kommandolinjeargumenter i Swift, må vi først importere `Foundation`-modulen. Deretter kan vi bruke funksjonen `CommandLine.arguments` som gir oss en liste over alle argumentene som er gitt når programmet ble kjørt. La oss se på et eksempel:

```Swift
import Foundation

// Hent ut argumentene fra kommandolinjen
let arguments = CommandLine.arguments

// Gå gjennom hvert argument og skriv ut dem
for arg in arguments {
    print(arg)
}
```

Lar vi nå kjøre programmet og gi det noen argumenter, for eksempel `swift example.swift arg1 arg2 arg3`, vil output bli følgende:

```
/Users/username/example.swift
arg1
arg2
arg3
```

Vi ser at det første argumentet i listen alltid vil være navnet på programmet, fulgt av eventuelle andre argumenter som er gitt.

## Deep Dive 
Det er også mulig å spesifisere hvilke argumenter som programmet forventer ved hjelp av en `switch`-setning. Dette kan være nyttig for å feilsøke ugyldige argumenter eller for å utføre ulike handlinger basert på hva brukeren har gitt av argumenter. La oss se på et eksempel der vi ønsker å kjøre ulike funksjoner basert på hvilket argument som er gitt:

```Swift
// Hent ut argumentene fra kommandolinjen
let arguments = CommandLine.arguments

// Sjekk hvilket argument som er gitt
switch arguments[1] {
case "hello":
    print("Hello world!")
case "bye":
    print("Goodbye!")
default:
    print("Invalid argument.")
}
```

La oss si at vi nå vil kjøre programmet med argumentet `swift example.swift hello` vil output bli `Hello world!`, mens om vi gir argumentet `swift example.swift bye` vil output bli `Goodbye!`. Dersom vi ikke gir noen av disse argumentene, vil det bli printet ut `Invalid argument.`

## Se Også
- [Swift Programmeringsguiden](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html)
- [Hvordan lese kommandolinjeargumenter i Swift](https://www.hackingwithswift.com/example-code/system/how-to-read-command-line-arguments-using-commandlinearguments)
- [CommandLine-dokumentasjon](https://developer.apple.com/documentation/foundation/commandline)