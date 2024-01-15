---
title:                "Kontrollera om en mapp finns"
html_title:           "Swift: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att checka om en mapp existerar är en viktig del av att hantera filsystemet i Swift. Det är ett enkelt sätt att säkerställa att dina applikationer fungerar som de ska och kan spara dig från onödiga fel och problem.

## Så här

Det finns flera sätt att checka om en mapp existerar i Swift, men det enklaste är att använda funktionen `fileExists(atPath:)`. Här är ett exempel:

```Swift
let fileManager = FileManager.default
let documentsPath = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!

let folderName = "MyFolder"

// Skapa en URL för mappen
let folderURL = documentsPath.appendingPathComponent(folderName)

// Checka om mappen existerar
if fileManager.fileExists(atPath: folderURL.path) {
    print("Mappen finns redan.")
} else {
    print("Mappen finns inte.")
}
```

I detta exempel använder vi FileManager för att skapa en URL för den önskade mappen. Sedan använder vi `fileExists(atPath:)` för att checka om mappen faktiskt existerar eller inte. Om mappen finns kommer vi att få ett meddelande som bekräftar det, annars kommer vi att få ett meddelande som säger att mappen inte finns.

## Djupdykning

För djupare kunskap om att checka mappar i Swift finns det ett par andra sätt att göra det på. En annan metod är att använda `fileExists(at:)`, som tar en URL som parameter istället för en sökväg. Det kan vara användbart om du redan har en URL för mappen du vill checka.

Det finns också möjligheten att använda `fileExists(atPath:)` för att checka en fil istället för en mapp. Detta kan vara användbart om du vill säkerställa att en viss fil finns innan du försöker hämta eller använda den.

## Se också

- [FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [URL](https://developer.apple.com/documentation/foundation/url)
- [Swift Standard Library](https://developer.apple.com/documentation/swift/swift_standard_library)