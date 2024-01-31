---
title:                "Skriva till standardfel"
date:                  2024-01-19
html_title:           "Arduino: Skriva till standardfel"
simple_title:         "Skriva till standardfel"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Standardfel (stderr) är en separat utström för att rapportera fel och loggmeddelanden, så att de inte blandas med huvudprogrammets utdata (stdout). Programmerare använder stderr till att isolera felmeddelanden och diagnostisk information, vilket underlättar felsökning och logghantering.

## Hur man gör:
```Swift
import Foundation

// Skriva till standardfel
func writeToStandardError(_ message: String) {
    if let data = "\(message)\n".data(using: .utf8) {
        FileHandle.standardError.write(data)
    }
}

// Använda funktionen
writeToStandardError("Ett fel inträffade.")

// Förväntad utskrift till standardfel:
// Ett fel inträffade.
```
Kör koden ovan för att se felmeddelandet i din konsol eller terminal.

## Fördjupning
Historiskt sett kommer begreppet stderr från Unix-operativsystemen där tre huvudströmmar definierades: stdin, stdout och stderr. Detta är standard inom många programmeringsspråk idag. Alternativt kan fel loggas till filer eller andra loggningstjänster, vilket kan vara användbart för storskaliga system. I Swift sker skrivning till stderr via `FileHandle.standardError` och detta är knutet till filbeskrivaren 2 på ett låg nivåsystem.

## Se även
- Swift API-dokumentation för `FileHandle`: [FileHandle | Apple Developer Documentation](https://developer.apple.com/documentation/foundation/filehandle)
- En översikt av standard I/O strömmar: [Standard streams - Wikipedia](https://en.wikipedia.org/wiki/Standard_streams)
