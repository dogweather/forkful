---
title:                "Søking og erstatning av tekst"
html_title:           "Lua: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å søke og erstatte tekst er en funksjon som finner spesifikke strenger i en tekst og erstatter dem med en ny. Som programmerere gjør vi det for å endre data, oppdatere informasjon eller rette feil.

## Hvordan Gjøre:

Her er et enkelt eksempel på hvordan du kan søke og erstatte tekst i Swift.

```Swift
var tekst = "Hallo, Verden!"
tekst = tekst.replacingOccurrences(of: "Verden", with: "Norge")
print(tekst)
```
Output:
```Swift
"Hallo, Norge!"
```
I dette eksempelet erstatter vi ordet "Verden" med "Norge".

## Dyp Dykk

Søk og erstatt-funksjonalitet har en lang historie som går tilbake til tidlig bruk av tekstbehandlingssystemer. I Swift er `replacingOccurrences(of:with:)` funksjonen en del av `Foundation` rammeverket, det gir en høy-nivå løsning for string manipulasjon.

Alternativt kan du også bruke `range(of:)` funksjonen for å finne området til strengen du vil erstatte og deretter bruke `replaceSubrange(_:with:)` for å erstatte teksten.

Detaljert, `replacingOccurrences(of:with:)` funksjonen fungerer ved å først opprette en kopie av originalstrengen. Deretter søker den gjennom kopien for hver forekomst av målstrengen. Når den finner en forekomst, erstatter den den med den nye strengen.

## Se Også:

Her er noen nyttige ressurser for å lære mer om søker og erstatter tekst i Swift:

- Apple's Swift Documentation: [Replacing Occurrences](https://developer.apple.com/documentation/foundation/nsstring/1416398-replacingoccurrences)
- Swift by Sundell: [Working with strings in Swift](https://www.swiftbysundell.com/basics/strings)
- Hacking with Swift: [How to replace parts of a string](https://www.hackingwithswift.com/example-code/strings/how-to-replace-parts-of-a-string-using-replacingoccurrences-of)