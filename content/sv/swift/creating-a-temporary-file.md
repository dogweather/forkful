---
title:                "Att skapa en temporär fil"
html_title:           "Swift: Att skapa en temporär fil"
simple_title:         "Att skapa en temporär fil"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skapande av en temporär fil är en vanlig handlingsform inom programmering. Det innebär att en fil skapas tillfälligt på datorns hårddisk under körningen av ett program och sedan raderas när den inte längre behövs. Detta används ofta för att hantera data som bara behövs tillfälligt eller för att utföra experimentella tester.

## Hur?
```Swift
let temporaryFile = FileManager.default.temporaryDirectory.appendingPathComponent("myFile.txt")
let data = "Hello World".data(using: .utf8)
FileManager.default.createFile(atPath: temporaryFile.path, contents: data, attributes: nil)
```
I detta exempel skapar vi en temporär fil med namnet "myFile.txt" i systemets temporära mapp och fyller den med texten "Hello World" i form av en Data-instans. Därefter används FileManager för att skapa filen och innehållet läggs till genom att ange sökvägen till den temporära filen och datan.

## Djupdykning
Skapandet av temporära filer har funnits sedan de tidiga dagarna av datorer. Det användes då för att spara tillfälliga data som program behövde för att kunna köra. Alternativ till att skapa en temporär fil är att använda operativsystemets minne för att hålla data, vilket kan vara mer effektivt men kan också vara mer komplicerat att implementera. Skapandet av en temporär fil följer vanligtvis en generell algoritm som består av att välja en unik filnamnstämpel, skapa och öppna filen och sedan använda den för att utföra önskade operationer.

## Se även
Önskar du läsa mer om skapandet av temporära filer i Swift? Kolla in dessa användbara källor:
- [Apple Developer Documentation](https://developer.apple.com/documentation/foundation/filemanager/1427163-createfile)
- [Swift by Sundell blogpost](https://www.swiftbysundell.com/basics/temporary-files/)