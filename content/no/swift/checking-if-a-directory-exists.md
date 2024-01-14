---
title:                "Swift: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Hvorfor

Som utvikler er det viktig å være i stand til å navigere og håndtere eksisterende filer og mapper. Å sjekke om en mappe eksisterer er en viktig del av dette, da det lar deg unngå feil og sikre en jevn kjøring av programmet ditt.

# Hvordan

For å sjekke om en mappe eksisterer i Swift, kan du bruke funksjonen `fileExists(atPath: String)` på `FileManager`-objektet. Denne funksjonen tar inn en filbane som en streng og returnerer en boolsk verdi som indikerer om filen eksisterer eller ikke. Se et eksempel i koden nedenfor:

```Swift
let fileManager = FileManager.default
let path = "/Users/brukernavn/Dokumenter/SwiftProjects/MinMappe"

if fileManager.fileExists(atPath: path) {
    print("Mappen eksisterer!")
} else {
    print("Mappen eksisterer ikke.")
}
```

I dette eksempelet bruker vi `FileManager`-objektet til å sjekke om mappen "MinMappe" eksisterer på den angitte banen. Hvis mappen eksisterer, skriver vi ut en melding som bekrefter dette, ellers skriver vi ut en annen melding.

# Dypdykk

I tillegg til å bruke `fileExists(atPath: String)`-funksjonen, kan du også bruke `fileExists(atPath: String, isDirectory: UnsafeMutablePointer<ObjCBool>)` for å sjekke om en mappe eksisterer. Denne funksjonen fungerer på samme måte som den første, men i tillegg kan du også få informasjon om hvorvidt den angitte banen peker til en mappe eller en fil. Dette kan være nyttig i noen tilfeller der du trenger å håndtere ulike typer filer.

Et annet aspekt å huske på når du sjekker for filer og mapper er at det kan være forskjeller mellom operativsystemene (OS) dine brukere kjører. For eksempel, hvis du utvikler et Swift-program for både macOS og iOS, må du ta hensyn til forskjellene i filsystemet på de to OS-ene. Du kan lese mer om dette i dokumentasjonen for `FileManager`.

# Se også

- [FileManager dokumentasjon](https://developer.apple.com/documentation/foundation/filemanager)
- [Sjekke om en fil eller mappe eksisterer i Swift](https://www.informit.com/article.asp?st=002722&seqNum=2)