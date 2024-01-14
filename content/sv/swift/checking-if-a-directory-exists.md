---
title:                "Swift: Att kolla om en mapp finns"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Det är viktigt att kunna kolla om en mapp existerar i Swift eftersom det är en avgörande del av många funktioner inom programmering. Genom att kunna kontrollera om en mapp existerar kan du utföra åtgärder som att skapa, läsa, uppdatera eller radera filer inuti mappen.

## Hur man gör

För att kontrollera om en mapp existerar i Swift, kan vi använda oss av FileManager-klassen. Vi behöver först ange sökvägen till mappen vi vill kontrollera. Sedan använder vi funktionen fileExists(atPath:) för att se om mappen faktiskt existerar. Nedan följer ett exempel som visar hur du kan kontrollera om en mapp vid namn "Blogg-inlägg" existerar i ditt dokumentbibliotek.

```Swift
// Definiera sökvägen till mappen
let directoryPath = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("Blogg-inlägg")

// Kontrollera om mappen existerar
if FileManager.default.fileExists(atPath: directoryPath.path) {
    print("Mappen existerar!")
} else {
    print("Mappen existerar inte.")
}
```

Om mappen existerar kommer konsollen att skriva ut "Mappen existerar!", annars skrivs "Mappen existerar inte."matt.exists(atPath:) returnerar en boolean-variabel och kan användas för att utföra olika åtgärder baserat på resultatet.

Om du vill skapa en mapp om den inte existerar, kan du använda dig av funktionen createDirectory(atPath:withIntermediateDirectories:attributes:). Den här funktionen tar även in en boolean-parameter som anger om mapparna i sökvägen ska skapas automatiskt om de inte existerar. Om du sätter den till true kommer mappen "Blogg-inlägg" att skapas om den inte redan finns.

## Djupdykning

När vi kontrollerar om en mapp existerar, tittar vi egentligen bara på om en fil vid sökvägen existerar. Detta kan innebära att sökvägen leder till en annan typ av fil, som till exempel en bildfil eller en textfil. Därför är det viktigt att kontrollera om filen vi letar efter faktiskt är en mapp innan vi utför några åtgärder på den.

Det är också viktigt att notera att sökvägen vi anger måste vara exakt och korrekt för att funktionen ska fungera. Om sökvägen är felaktig kommer funktionen att returnera false, även om mappen faktiskt existerar.

## Se även

- [Apple Developer - FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Stack Overflow - Checking if a directory exists in Swift](https://stackoverflow.com/questions/3437897/checking-if-a-directory-exists-in-a-unix-shell-script)