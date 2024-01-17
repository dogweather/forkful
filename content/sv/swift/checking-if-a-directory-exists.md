---
title:                "Kolla om en mapp existerar"
html_title:           "Swift: Kolla om en mapp existerar"
simple_title:         "Kolla om en mapp existerar"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en mapp finns är en viktig del av programmering eftersom det möjliggör för utvecklare att hantera olika scenarier som kan uppstå under körning av ett program.

## Så här gör du:
```Swift
if let directoryPath = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first {
    if FileManager.default.fileExists(atPath: directoryPath.path) {
        print("Mappen finns!")
    } else {
        print("Mappen finns inte.")
    }
}
```
Sample output:
```Swift
Mappen finns!
```

## Djupdykning:
Att kontrollera om en mapp finns är en viktig del av filhantering i Swift. Det gör det möjligt för utvecklare att hantera olika scenarier som kan uppstå under körning av ett program, till exempel när man försöker skriva till en mapp som inte finns på enheten. Alternativet till att använda FileManager för att kontrollera mappens existens är att använda metoder som fileExists() eller fileExistsAtPath() från NSString eller NSArray klasserna.

För att kontrollera mappens existens används en instans av FileManager klassen, vilket ger tillgång till filsystemet på enheten. Med hjälp av metoden urls(for:in:) kan man specifikt hämta sökvägen till dokumentmappen i enhetens sandbox. Genom att använda metoden fileExists(atPath:) kan man sedan kontrollera om sökvägen leder till en befintlig mapp eller inte.

## Se även:
- [Apple Developer Documentation - FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Apple Developer Documentation - NSString](https://developer.apple.com/documentation/foundation/nsstring)
- [Apple Developer Documentation - NSArray](https://developer.apple.com/documentation/foundation/nsarray)
- [Swift By Sundell - Filhantering i Swift](https://www.swiftbysundell.com/basics/file-handling/)