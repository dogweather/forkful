---
title:                "Swift: Sjekke om en mappe eksisterer"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du jobber med Swift-programmering, vil du kanskje lure på hvorfor det er viktig å sjekke om en mappe eksisterer. Å kontrollere om en mappe allerede finnes er viktig for å sikre at koden din kjører uten feil og for å håndtere potensielle problemer med å lage en ny mappe som allerede finnes.

## Hvordan
For å sjekke om en mappe eksisterer i Swift, kan du bruke funksjonen `FileManager.default.fileExists(atPath:)`. Denne funksjonen tar en streng som argument og returnerer en bool-verdi som indikerer om mappen eksisterer eller ikke. Se på kodeeksempelet nedenfor for å se hvordan du kan bruke denne funksjonen:

```Swift
let fileManager = FileManager.default
let directoryPath = "Dokumenter/minMappe"
if fileManager.fileExists(atPath: directoryPath) {
    print("Mappen eksisterer allerede")
} else {
    print("Mappen eksisterer ikke")
}
```
I dette eksempelet bruker vi `FileManager`-klassen til å sjekke om en mappe ved navn "minMappe" eksisterer i mappen "Dokumenter". Hvis mappen finnes, vil utskriften være "Mappen eksisterer allerede", ellers vil den være "Mappen eksisterer ikke".

## Dykk dypere
For å forstå hvordan `fileExists(atPath:)` fungerer, er det viktig å vite at denne funksjonen bruker en absolutt eller relativ sti til mappen. En absolutt sti starter alltid fra roten av filsystemet, mens en relativ sti starter fra gjeldende mappe. Hvis du vil bruke en relativ sti, må du først finne den gjeldende stien din med `FileManager.default.currentDirectoryPath`.

## Se også
- [Swift Dokumentasjon - File Manager](https://developer.apple.com/documentation/foundation/filemanager)
- [Hvordan sjekke om en fil eller mappe eksisterer i Swift](https://matteomanzinello.com/how-to-check-if-a-file-or-folder-exists-in-swift/)
- [FileManager - Sjekk om fil eller mappe eksisterer](https://wwdcnotes.com/notes/wwdc19/233/)