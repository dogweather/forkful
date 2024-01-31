---
title:                "Kontrollera om en katalog finns"
date:                  2024-01-20T14:58:38.891686-07:00
html_title:           "Fish Shell: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"

category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en mapp finns är att se till att en väg pekar på en mapp som faktiskt existerar i filsystemet. Programmerare gör detta för att undvika fel som kan uppstå när man försöker komma åt eller modifiera filer i en icke-existerande mapp.

## Hur gör man:
Swift ger oss `FileManager` för att hantera filsystemoperationer. Så här ser du snabbt om en mapp finns:

```Swift
import Foundation

let fileManager = FileManager.default
let directoryPath = "/path/to/your/directory"

if fileManager.fileExists(atPath: directoryPath) {
    print("Mappen finns!")
} else {
    print("Mappen finns inte.")
}
```

Kör du detta kommer du att få output beroende på om mappen finns eller inte:

```
Mappen finns!
```

eller

```
Mappen finns inte.
```

## Djupdykning
Kontrollen om en mapp finns är viktig både för skriv- och läsoperationer och har varit det sedan tidiga datordagar. Utan denna kontroll kan program försöka utföra operationer på felaktiga platser, vilket kan resultera i krascher eller dataskador.

Alternativ till `FileManager` kan inkludera att använda terminalkommandon via `Process` men detta är mindre säkert och portabelt än att använda inbyggda Swift-API:er. 

`fileExists(atPath:)` kontrollerar inte bara om mappen finns, den kan också berätta om vägen är en mapp eller en fil. Detta görs genom att använda `isDirectory` parameter:

```Swift
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: directoryPath, isDirectory: &isDirectory) {
    if isDirectory.boolValue {
        print("Det är en mapp.")
    } else {
        print("Det är en fil.")
    }
} else {
    print("Mappen eller filen finns inte.")
}
```

Ett förhållningssätt till att kontrollera om en mapp faktiskt är en mapp och inte en fil är viktigt om du vill interagera med filsystemet på ett förutsebart sätt.

## Se även
- Apple's dokumentation om `FileManager`: [FileManager - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/filemanager)

Dessa länkar leder till detaljerade guider och dokumentation som kan hjälpa till att utöka användningen av filsystemhantering i Swift.
