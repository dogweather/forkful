---
title:                "Att skapa en tillfällig fil"
html_title:           "Bash: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att skapa en temporär fil innebär att skapa en flyktig databärare. Programmerare gör det för att lagra data temporärt--data som vi kanske inte vill spara eller ha tillgängliga långsiktigt, såsom cache- och bufferdata.

## Hur göra:
Här kommer en grundläggande kod för att skapa en temporär fil i Swift:

```Swift
import Foundation

let tempDir = NSTemporaryDirectory()
let tempFile = "temp.txt"
let tempPath = tempDir + tempFile

do {
    try "Några tillfälliga data".write(toFile: tempPath, atomically: true, encoding: .utf8)
    print("Temporär fil skapad på sökvägen: \(tempPath)")
} catch {
    print("Fel vid skapandet av filen: \(error)")
}
```
Kör du koden ovan, får du något i stil med:
```
Temporär fil skapad på sökvägen: /var/folders/l4/.../T/temp.txt
```
Det indikerar att din temporära fil skapats framgångsrikt.

## Fördjupning:
Historiskt sett har temporära filer varit viktiga för att hantera begränsningar i minneskapacitet. De hjälper till att optimera utrymmet genom att bara hålla data som omedelbart behövs.

Alternativ till temporära filer kan vara hårddiskpartitioner eller minnesplatser, beroende på när datalagring behövs. Vissa programmerare kan också använda in-memory databaser som Redis för att lagra tillfällig information.

När det gäller att skapa temporära filer i Swift, använder vi `NSTemporaryDirectory()` funktionen, som returnerar sökvägen till det tillfälliga katalogsområdet för den aktuella användaren. Till skillnad från traditionella filsystem garanterar inte systemet bevarandet av dessa filer och de kan tas bort vid vilken tidpunkt som helst.

## Se också:
Kolla in dessa resurser för mer information om att hantera tillfälliga filer i Swift och andra programmeringsspråk:
- [Apple Developer Documentation: NSTemporaryDirectory()](https://developer.apple.com/documentation/foundation/1413049-nstemporarydirectory)
- [StackOverflow: Best way to create temporary/temp files in Java and .NET?](https://stackoverflow.com/questions/178046/best-way-to-create-temporary-temp-files-in-java-and-net)
- [Wikipedia: Temporary folder](https://en.wikipedia.org/wiki/Temporary_folder)