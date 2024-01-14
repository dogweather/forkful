---
title:                "Swift: Lesing av kommandolinje-argumenter"
simple_title:         "Lesing av kommandolinje-argumenter"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du er en utvikler som ønsker å lage et program som kan kommunisere og ta imot input fra brukeren gjennom terminalen, er det viktig å kunne lese kommandolinjeargumenter. Dette vil tillate deg å utvide funksjonaliteten til programmet ditt og gjøre det mer interaktivt.

## Slik gjør du det
For å kunne lese kommandolinjeargumenter må du først importere Foundation-biblioteket. Deretter kan du bruke funksjonen `CommandLine.arguments` for å få tilgang til alle argumentene som ble sendt med ved oppstart av programmet. Her er et eksempel på hvordan du kan skrive ut argumentene og antall argumenter i terminalen:

````Swift
import Foundation

let arguments = CommandLine.arguments
print("Her er argumentene som ble sendt med: \(arguments)")
print("Antall argumenter: \(arguments.count)")
````

Dette vil gi følgende output i terminalen:

````Console
Her er argumentene som ble sendt med: ["programnavn", "argument1", "argument2"]
Antall argumenter: 3
````
Det er også mulig å få tilgang til individuelle argumenter ved å bruke indeksering. For eksempel, hvis du vil få tak i det første argumentet, kan du bruke `CommandLine.arguments[1]`.

## Dypdykk
Det er flere teknikker for å lese og behandle kommandolinjeargumenter mer avansert. En av disse er å bruke argumentflagg, også kjent som "options". Dette lar deg spesifisere en spesiell handling eller funksjon for programmet ditt basert på hvilke flagg som blir sendt med som argumenter. For å gjøre dette må du bruke `CommandLine.Option` og `CommandLine.OptionSet` fra Foundation-biblioteket.

Et annet viktig konsept er å validere argumentene du mottar. Dette er spesielt viktig hvis du trenger å behandle sensitiv informasjon gjennom kommandolinjen. Ved å bruke `CommandLine.CommandLineError` fra Foundation-biblioteket, kan du håndtere eventuelle ugyldige input og gi tilbakemelding til brukeren.

## Se også
- [Apple Documentation: CommandLine](https://developer.apple.com/documentation/foundation/commandline)
- [Swift by Sundell: Working with the Swift command line](https://www.swiftbysundell.com/posts/working-with-the-swift-command-line)
- [Hacking with Swift: Command line arguments](https://www.hackingwithswift.com/articles/162/command-line-arguments-in-swift)