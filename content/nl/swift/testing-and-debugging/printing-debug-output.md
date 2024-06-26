---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:49.077756-07:00
description: 'Hoe te: In Swift heb je een vriend aan de `print()` functie. Makkelijk
  te gebruiken, het geeft je ogen op wat er in je code gebeurt.'
lastmod: '2024-03-13T22:44:51.159537-06:00'
model: gpt-4-0125-preview
summary: In Swift heb je een vriend aan de `print()` functie.
title: Debug-output afdrukken
weight: 33
---

## Hoe te:
In Swift heb je een vriend aan de `print()` functie. Makkelijk te gebruiken, het geeft je ogen op wat er in je code gebeurt.

```Swift
var groet = "Hallo, speelplaats"
print(groet)
// Output: Hallo, speelplaats

let nummers = [1, 2, 3, 4, 5]
for nummer in nummers {
    print(nummer)
}
// Output:
// 1
// 2
// 3
// 4
// 5
```

Maar wacht, er is meer! Heb je gedetailleerde debug-info nodig? `debugPrint()` heeft je gedekt:

```Swift
debugPrint(groet)
// Output: "Hallo, speelplaats"
```

Zie je die aanhalingstekens? `debugPrint()` laat de bonen morsen met extra details over gegevenstypes en -structuur.

## Diepe Duik
In de oude dagen van Objective-C gebruikten we `NSLog` om spullen te loggen. Swift hield de zaken eenvoudig—`print()` is je brood en boter voor standaard output, terwijl `debugPrint()` de gekruide boter is voor gedetailleerde weergaven.

Interessant feit: Standaard output in Swift is niet alleen tekst—het kan elk type zijn dat voldoet aan `CustomStringConvertible` of `CustomDebugStringConvertible`. Deze protocollen laten je aanpassen hoe je objecten eruit zien wanneer ze hun verhalen vertellen door te printen.

Onder de motorkap gebruiken `print()` en `debugPrint()` `String(describing:)` en `String(reflecting:)` om je objecten in strings om te zetten. In principe gebruiken deze functies een spiegel om een selfie van je gegevens te nemen.

Alternatieven? Je hebt `os_log` en `NSLog`, maar deze zijn meer geschikt voor productie-niveau logging, niet het snelle en vuile debuggen waar we hier op jammen.

## Zie Ook
- Apple's Swift API-referentie voor printfuncties: [Swift Standard Library: print(_:separator:terminator:)](https://developer.apple.com/documentation/swift/1541053-print)
- Een diepere kijk op loggen in Swift, GDPR en privacy overwegingen: [Unified Logging and Activity Tracing](https://developer.apple.com/documentation/os/logging)
- Swift's string interpolatie en aanpasbaarheid voor debugbeschrijvingen: [CustomStringConvertible](https://developer.apple.com/documentation/swift/customstringconvertible) en [CustomDebugStringConvertible](https://developer.apple.com/documentation/swift/customdebugstringconvertible)
