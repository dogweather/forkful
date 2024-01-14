---
title:                "Swift: Läsning av en textfil"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil är en grundläggande och viktig färdighet inom Swift-programmering. Genom att kunna läsa och hantera textfiler kan du lättare arbeta med stora mängder data och skapa mer interaktiva och dynamiska program. I den här bloggposten kommer vi att titta på hur man läser en textfil i Swift.

## Så här gör du

För att läsa en textfil i Swift behöver du först skapa en instans av klassen `FileHandle`. Du kan då använda metoden `readDataToEndOfFile()` för att läsa allt innehåll från filen och returnera det som en array av typen `Data`. Sedan kan du använda `String`-konstruktorn för att omvandla datan till textformat.

````Swift
// Skapa en instans av FileHandle
let fileHandle = FileHandle(forReadingAtPath: "myFile.txt")

// Läsa in all data från filen
let fileData = fileHandle?.readDataToEndOfFile()

// Konvertera till textformat
let fileText = String(data: fileData!, encoding: .utf8)
````

Om du vill läsa filen rad för rad istället för allt på en gång kan du använda metoden `readData(ofLength:)`. Den tar in ett `Int` värde som anger antal bytes som ska läsas från filen. Om vi vet att vårt textdokument består av en rad per 100 bytes kan vi använda följande kod:

````Swift
// Skapa en instans av FileHandle
let fileHandle = FileHandle(forReadingAtPath: "myFile.txt")

// Läsa en rad från filen
let fileData = fileHandle?.readData(ofLength: 100)

// Konvertera till textformat
let fileText = String(data: fileData!, encoding: .utf8)
````

## Djupdykning

När du läser en textfil i Swift är det viktigt att tänka på hur texten är kodad i filen. Om filen är kodad i ett annat format än UTF-8, till exempel ASCII, måste du ange den korrekta kodningen när du använder `String`-konstruktorn. Annars kan det leda till fel och korrupt data.

En annan viktig aspekt att tänka på är filens placering. Om du vill läsa en fil som finns i samma mapp som ditt Swift-projekt kan du bara ange filnamnet som vi gjorde i exempelkoden ovan. Om filen är placerad i en annan mapp måste du ange hela sökvägen till filen.

## Se även

Även om det är viktigt att kunna läsa en fil, är det lika viktigt att kunna skriva till en fil. Om du vill lära dig hur du kan göra det kan du läsa vår tidigare bloggpost om hur man skriver till en textfil i Swift.

Läs mer om FileHandle-klassen i Apples officiella dokumentation:

https://developer.apple.com/documentation/foundation/filehandle

Läs om hur man arbetar med textfiler i Swift Playgrounds:

https://developer.apple.com/documentation/swift_playgrounds/introduction