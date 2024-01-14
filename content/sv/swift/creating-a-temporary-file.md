---
title:                "Swift: Skapa en temporär fil"
simple_title:         "Skapa en temporär fil"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Många gånger när vi programmerar behöver vi skapa tillfälliga filer för att lagra data eller göra temporära beräkningar. Detta kan vara till nytta i en mängd olika användningsområden, som till exempel när vi behöver hantera stora datamängder eller när vi vill göra tester medan vi utvecklar vår kod. I denna bloggpost kommer vi att ta en djupdykning i hur man skapar tillfälliga filer i Swift och hur de kan användas.

## Så här gör du

För att skapa en tillfällig fil i Swift använder vi `FileManager` och `URL` klasserna. Låt oss följa stegen nedan för att skapa en temporär fil och sedan skriva till den:

```Swift
// Skapa en unik URL för vår tillfälliga fil
let tempURL = URL(fileURLWithPath: NSTemporaryDirectory()).appendingPathComponent("temp.txt")

// Skapa en FileManager instans
let fileManager = FileManager.default

// Skapa en tom fil på den unika URL:en
fileManager.createFile(atPath: tempURL.path, contents: nil, attributes: nil)

// Skriv till vår temporära fil
let text = "Hej, detta är en temporär fil!"
do {
    try text.write(to: tempURL, atomically: true, encoding: .utf8)
} catch {
    print(error.localizedDescription)
}

// Läs från vår temporära fil
do {
    let tempText = try String(contentsOf: tempURL, encoding: .utf8)
    print(tempText)
} catch {
    print(error.localizedDescription)
}
```

Om vi kör detta kodblock så kommer vi att få följande output:

```
Hej, detta är en temporär fil!
```

Som vi kan se har vi nu skapat en tillfällig fil och lyckats skriva samt läsa från den. Men vad händer egentligen bakom kulisserna?

## Djupdykning

När vi skapar en tillfällig fil i Swift så tilldelas den en unik URL genom `NSTemporaryDirectory()` funktionen. Detta är en standardmapp på vårt system som är reserverad för tillfälliga filer. Sedan använder vi `FileManager` för att skapa själva filen på den unika URL:en. Genom att sätta innehållet till `nil` så skapas en tom fil.

Nästa steg är att skriva till vår temporära fil. Här använder vi `write(to:atomically:encoding:)` funktionen från `String` klassen för att skriva önskat innehåll till vår fil. Slutligen använder vi `String(contentsOf:encoding:)` funktionen för att läsa från vår fil och skriva ut innehållet.

Det finns också flera andra alternativ för att skapa tillfälliga filer i Swift, såsom att använda `FileHandle` eller `URLSessionDataTask` klasserna. Det är alltid bra att utforska olika lösningar och hitta det som passar bäst för ens nuvarande användningsområde.

## Se även

- [Apple Developer Dokumentation för FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [En guide till temporära filer i Swift](https://medium.com/better-programming/ultimate-guide-to-temporary-files-in-swift-9272a3b17fe1)
- [Skapa temporära filer i Swift](https://theswiftdev.com/create-temporary-files-in-swift/)