---
title:                "Skriver til standard feil"
html_title:           "Swift: Skriver til standard feil"
simple_title:         "Skriver til standard feil"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor skulle noen bry seg med å skrive til standard feil i koden sin? Vel, det kan høres ut som en unødvendig trinn, men det kan føre til en bedre og mer effektiv koding.

## Hvordan
Å skrive til standard feil i Swift er ganske enkelt. Alt du trenger å gjøre er å bruke "print" funksjonen og sette "stderr" parameteren til sann. Dette vil sende utdata til standard feil i stedet for standard utgang. Her er et eksempel på hvordan du gjør det:

```Swift
print("Dette vil bli skrevet til standard feil", toStream: &stderr)
```

Når du kjører denne koden, vil "Dette vil bli skrevet til standard feil" vises i terminalen din, men hvis du logger standard utgang, så vil det ikke vises. Her er et eksempel på hvordan du logger standard feil til en fil i stedet:

```Swift
let file = FileHandle(forWritingAtPath: "errorLog.txt")!
print("Dette vil bli skrevet til standard feil", toStream: &stderr)
```

Dette vil skrive utdata til filen "errorLog.txt" i stedet for terminalen.

## Dypdykk
Nå lurer du kanskje på hvorfor det er nyttig å skrive til standard feil i stedet for standard utgang. Vel, når du håndterer unntak i koden din, kan det være nyttig å skrive feilmeldinger til standard feil i stedet for standard utgang. På denne måten vil du kunne skille mellom vanlige utdata og feilmeldinger når du kjører koden din.

En annen grunn til å skrive til standard feil er at det kan hjelpe deg med å diagnostisere og løse problemer i utviklingsprosessen. Ved å skrive feilmeldinger til standard feil, vil du kunne finne ut hvor og hvorfor koden din krasjer eller ikke fungerer som forventet.

For å lese mer om hvordan man håndterer unntak i Swift og bruker standard feil, kan du sjekke ut følgende lenker:

- [Håndtere unntak i Swift](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- [Standard Library Reference for Printing Functions](https://developer.apple.com/documentation/swift/foundation/1540999-print)
- [Swift API Reference for FileHandle](https://developer.apple.com/documentation/foundation/filehandle)

## Se også
- [Hvordan håndtere unntak i Swift](https://www.hackingwithswift.com/read/0/22/handling-errors)
- [En introduksjon til Swift-programmering](https://www.appcoda.com/swift-intro/)