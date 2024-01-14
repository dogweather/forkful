---
title:    "Swift: Skapa en tillfällig fil"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Att skapa en tillfällig fil kan vara användbart när du behöver lagra temporära data under körningstiden av ditt program. Det är också en vanlig praxis när du arbetar med stora datamängder och vill läsa och skriva från dem på ett effektivt sätt.

## Hur man gör

För att skapa en tillfällig fil i Swift kan du använda dig av klassen `NSTemporaryDirectory` tillsammans med `FileManager`. Här är en grundläggande kodexempel:

```Swift
// Skapa en unik filnamn med hjälp av tidpunkten för nuvarande datum
let fileName = "myTempFile_\(Date().timeIntervalSince1970)"
// Skapa en tillfällig fil med hjälp av FileManager
let tempFileURL = URL(fileURLWithPath: NSTemporaryDirectory()).appendingPathComponent(fileName)
// Skriv till filen
try "Hello, world!".write(to: tempFileURL, atomically: true, encoding: .utf8)
// Läs från filen
let contents = try String(contentsOf: tempFileURL)
// Skriv ut innehållet
print(contents) // Hello, world!
```

## Deep Dive

När du använder `NSTemporaryDirectory()` för att skapa en tillfällig fil, så kommer den att skapas i ett temporärt katalog som hanteras av operativsystemet. Det är viktigt att notera att filen kommer att tas bort automatiskt när programmet avslutas. Om du behöver behålla filen längre än så kan du flytta den till en annan katalog eller använda dig av `URL`-objektets `isTemporary`-egenskap för att kontrollera om filen är tillfällig när du behöver ta bort den manuellt.

## Se även

- [Apple Developer Documentation - NSTemporaryDirectory](https://developer.apple.com/documentation/foundation/filemanager/1407694-nstemporarydirectory)
- [Swift by Sundell - Creating temporary files in Swift](https://www.swiftbysundell.com/articles/creating-temporary-files-in-swift/)