---
title:                "Lesing av kommandolinjeargumenter"
html_title:           "Swift: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor skulle noen bry seg om å lese kommandolinjeargumenter når man koder i Swift? Vel, det er en enkel måte å gi dine programmer mer fleksibilitet og dynamisk funksjonalitet på. Du kan lese argumenter som brukeren skriver inn og deretter tilpasse programmet ditt basert på disse argumentene. Dette åpner opp for mange muligheter, som å kjøre forskjellige funksjoner avhengig av argumentene, eller endre programinnstillinger basert på brukerens preferanser.

# Slik gjør du det

For å lese kommandolinjeargumenter i Swift, kan du bruke `CommandLine`-klassen. Først må du importere Foundation-frameworket, som inneholder denne klassen. Deretter kan du bruke `CommandLine.arguments`-egenskapen for å få en liste over alle argumentene som ble gitt til programmet ditt.

Her er et eksempel på hvordan du kan skrive ut alle argumentene til konsollen:

```Swift
import Foundation

// Skriv ut alle argumentene
print(CommandLine.arguments)

// Kjør dette programmet fra Terminalen med noen argumenter, for eksempel:
// swift myProgram.swift arg1 arg2 arg3
// Resultatet vil bli ["myProgram.swift", "arg1", "arg2", "arg3"]
```

Som du kan se, vil det første argumentet alltid være navnet på programmet, etterfulgt av alle de andre argumentene som ble gitt.

# Dypdykk

Nå som du vet hvordan du kan få tilgang til kommandolinjeargumentene, kan du begynne å bruke dem til å tilpasse ditt eget program. En måte å gjøre dette på er å bruke `if`-setninger for å sjekke hvilket argument som ble gitt, og utføre forskjellige handlinger basert på det.

Her er et eksempel på hvordan du kan bruke kommandolinjeargumenter til å endre språkinnstillingene til programmet ditt:

```Swift
import Foundation

// Sjekk om argumentet "language" ble gitt
if CommandLine.arguments.contains("language") {
  // Finn indeksen til argumentet "language"
  if let index = CommandLine.arguments.firstIndex(of: "language") {
    // Bruk argumentet som kommer etter "language" som et nytt språk
    let newLanguage = CommandLine.arguments[index+1]
    // Endre programinnstillingene til å bruke det nye språket
    changeLanguage(to: newLanguage)
  }
}
```

Dette er bare ett eksempel, men det er mange andre måter du kan bruke kommandolinjeargumenter på for å gjøre programmene dine mer dynamiske og tilpassbare.

# Se også

- [Dokumentasjon om `CommandLine`-klassen](https://developer.apple.com/documentation/foundation/commandline) fra Apple.
- [En introduksjon til kommandolinjeargumenter i Swift](https://www.raywenderlich.com/817037-command-line-arguments-in-swift) fra Ray Wenderlich.
- [En guide til å lage konsollapplikasjoner i Swift](https://www.hackingwithswift.com/articles/195/how-to-make-a-command-line-application-using-swift) fra Hacking with Swift.