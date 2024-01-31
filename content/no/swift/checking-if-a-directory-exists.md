---
title:                "Sjekke om en mappe eksisterer"
date:                  2024-01-20T14:58:53.833421-07:00
simple_title:         "Sjekke om en mappe eksisterer"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Å sjekke om en mappe eksisterer betyr å verifisere at en spesifikk sti på filsystemet ditt peker til en faktisk mappe. Programmerere gjør dette for å unngå feil ved filhåndtering, som å forsøke å lese fra eller skrive til en ikke-eksisterende mappe.

## How to: (Hvordan:)
Swift gir oss `FileManager` for å håndtere filsystemoperasjoner. Her er hvordan du sjekker om en mappe eksisterer:

```Swift
import Foundation

let fileManager = FileManager.default
let path = "/some/path/to/directory"

if fileManager.fileExists(atPath: path, isDirectory: nil) {
    print("Mappen eksisterer!")
} else {
    print("Mappen eksisterer ikke.")
}
```

Output kan være:

```
Mappen eksisterer!
```

Eller:

```
Mappen eksisterer ikke.
```

## Deep Dive (Dypdykk)
Før `FileManager` var NSFileManager standarden i Objective-C. Swift forenklet prosessen med høyere nivå API'er. Det finnes alternativer som å bruke `URL`-objekter og `fileSystemRepresentation` for mer robust håndtering av sti-tegnkodinger. Ved sjekking med `fileExists(atPath:)`, husk at funksjonen returnerer `true` for både filer og mapper, så for å spesifikt bekrefte en mappe, bruk `isDirectory` parameteret.

```Swift
var isDir: ObjCBool = false
if fileManager.fileExists(atPath: path, isDirectory: &isDir) {
    if isDir.boolValue {
        // Det er bekreftet å være en mappe
    } else {
        // Stien eksisterer, men det er en fil, ikke en mappe
    }
}
```

Ved å referere til `isDir`, kan vi skille mellom filer og mapper.

## See Also (Se også)
- [Apple FileManager Documentation](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift API Design Guidelines](https://www.swift.org/documentation/api-design-guidelines/)
- [File System Basics](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/FileSystemOverview/FileSystemOverview.html)
