---
title:                "Läsning av en textfil"
html_title:           "Swift: Läsning av en textfil"
simple_title:         "Läsning av en textfil"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att läsa en textfil innebär att föra in det innehåll som finns i en textfil till en dator och göra den tillgänglig för programmet som körs. Programmerare gör detta för att kunna använda sig av innehållet i textfilen i sina program, till exempel för att lagra data eller läsa in text för att skriva ut eller manipulera.

## Hur man gör:

För att läsa en textfil i Swift använder man sig av klassen `FileManager` och metoden `contents(atPath: String)`. Detta returnerar innehållet i form av en binär dataström som kan konverteras till en `String`. Här är ett exempel:

```Swift
do {
    let fileURL = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("textfil.txt")
    let fileContent = try String(contentsOf: fileURL, encoding: .utf8)
    print(fileContent)
} catch {
    print("Kunde inte läsa filen")
}
```

👉 **Output:** Innehållet i `textfil.txt` skrivs ut på konsolen.

## Djupdykning:

Att läsa textfiler är en vanlig uppgift för programmerare eftersom det ofta är ett sätt att lagra och hantera data i deras program. Innan Swift fanns var det vanligt att använda lägre nivå-gränssnitt som `fopen()` och `fread()`, men nu har man förenklat processen med `FileManager`. Det finns också andra sätt att läsa textfiler, till exempel med `InputStream` för större filer eller med hjälp av `Data(contentsOf: URL)` för att få en binär dataström att arbeta med.

## Se även:

- [Apple's Dokumentation för FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Tutorial: Working with Files in Swift](https://www.raywenderlich.com/-levels-of-swift/1967201-tutorial-working-with-files-in-swift)