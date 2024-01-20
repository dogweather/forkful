---
title:                "Läsa en textfil"
html_title:           "Fish Shell: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att läsa en textfil innebär att extrahera den information som lagras i en fil i textformat. Programmerare gör detta för att hitta, använda och bearbeta specifik data utan att redigera själva filen.

## Hur Man Gör:

Här är ett exempel på hur du läser en .txt-fil i Swift:

```Swift
import Foundation

let fileURL = Bundle.main.url(forResource: "Example", withExtension: "txt")
do {
    let content = try String(contentsOf: fileURL!, encoding: .utf8)
    print(content)
} catch {
    print("Kunde inte läsa filen")
}
```
Om "Example.txt" innehåller "Hej, Världen!", kommer output att se ut som:

```Swift
Hej, Världen!
```

## Djupdykning:

Historiskt sett har åtkomst till filsystem varit en viktig del av programmering. Olika programmeringsspråk hanterar filinläsning på olika sätt, men Swift gör det snabbt och smidigt.

Ett alternativ är att använda `FileHandle` istället för `String(contentsOf: ...)`. Det tillåter dig att manipulera filens 'read-pointer': vilket hjälper vid inläsning av stora filer.

Ett annat detalj att notera är att Swift är casesensitive, vilket innebär att "Example.txt" och "example.txt" behandlas som två olika filer.

## Se Also:

För vidare läsning och resurser, se följande länkar:

- [Apple Filhantering](https://developer.apple.com/documentation/foundation/filemanager)
- [Apple Swift Documentation](https://developer.apple.com/documentation/swift)
- [Swift Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [File Handling in Swift with FileManager](https://www.raywenderlich.com/7181017-file-handling-in-swift-with-filemanager)