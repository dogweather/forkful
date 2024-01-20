---
title:                "Sjekker om en mappe eksisterer"
html_title:           "Lua: Sjekker om en mappe eksisterer"
simple_title:         "Sjekker om en mappe eksisterer"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Sjekker om en Katalog Eksisterer i Swift

## Hva og Hvorfor?

Sjekker om en katalog eksisterer innebærer at vi leter etter en spesifikk katalog i et filsystem. Dette gjør vi for å unngå feil som kan oppstå hvis det vi prøver å lese eller skrive til en katalog som ikke finnes.

## Hvordan Gjør vi Det?

I Swift er det veldig rett fram å sjekke om en katalog eksisterer.

```Swift
import Foundation

let fileManager = FileManager.default
let directoryPath = "/path/to/your/directory"

if fileManager.fileExists(atPath: directoryPath) {
    print("Katalogen eksisterer")
} else {
    print("Katalogen eksisterer ikke")
}
```

Kjører du dette eksemplet og bytter `/path/to/your/directory` med en faktisk sti, vil du se stringen "Katalogen eksisterer" eller "Katalogen eksisterer ikke" printet ut i terminalen din.

## Fordypning

Denne metoden har lenge vært et grunnleggende verktøy i programmering siden tidlige dager av UNIX. Alternativer inkluderer bruk av `attributesOfItem(atPath:)` metoden, men det gir mer informasjon enn bare om katalogen eksisterer. Det er verdt å merke seg at `fileExists(atPath:)` metoden også sjekker om filer eksisterer, så navnet kan være misvisende.

## Se Også

For mer informasjon om `FileManager` og relaterte metoder, sjekk ut disse lenkene:  
1. [Apple Dokumentasjon on FileManager](https://developer.apple.com/documentation/foundation/filemanager)
2. [Stack Overflow: How to check if a directory exists in Swift](https://stackoverflow.com/questions/30089775/how-to-check-if-a-directory-exists-in-swift) 
3. [Apple Dokumentasjon on fileExists(atPath:)](https://developer.apple.com/documentation/foundation/filemanager/1410277-fileexists)

*Artikkelen har ingen konklusjonsdel.*