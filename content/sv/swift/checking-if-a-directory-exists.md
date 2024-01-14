---
title:    "Swift: Kontrollera om en mapp finns"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp finns kan vara mycket användbart inom Swift programmering. Det kan hjälpa till att säkerställa att programmet fortsätter köra på ett smidigt sätt och hantera eventuella felaktiga eller saknade mappar.

## Hur man gör

För att kontrollera om en mapp finns i Swift, kan du använda funktionen `fileExists()` från `FileManager`. Du behöver först ange sökvägen till mappen och sedan använda `fileExists()` för att returnera en boolesk utgång med värdet `true` om mappen finns och `false` om den inte finns.

```Swift
let fileManager = FileManager.default
let directory = "/Users/John/Documents"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: directory, isDirectory: &isDirectory) {
    if isDirectory.boolValue {
        print("Mappen finns!")
    } else {
        print("Sökvägen pekar till en fil, inte en mapp.")
    }
} else {
    print("Mappen finns inte.")
}
```

Om mappen inte finns eller sökvägen pekar på en fil, kommer du att få respektive meddelande. Du kan också lägga till ytterligare villkor och åtgärder baserat på resultatet av `fileExists()`-funktionen.

## Djupdykning

Om du vill utföra en ännu mer detaljerad kontroll av en mapp, kan du använda funktionen `attributesOfItem()` från `FileManager`. Detta returnerar en dictionary som innehåller information om filen eller mappen, inklusive storlek, datum för senaste ändring och behörigheter.

```Swift
do {
    let fileAttributes = try fileManager.attributesOfItem(atPath: directory)
    let fileSize = fileAttributes[FileAttributeKey.size]
    let fileModificationDate = fileAttributes[FileAttributeKey.modificationDate]
    let filePermissions = fileAttributes[FileAttributeKey.posixPermissions]
    
    print("Mappstorlek: \(fileSize!)")
    print("Senaste ändring: \(fileModificationDate!)")
    print("Behörigheter: \(filePermissions!)")
} catch {
    print("Kunde inte hämta attribut för mappen.")
}
```

Dessa attribut kan vara användbara för att utföra specifika uppgifter inom din kod, till exempel att kontrollera när en mapp senast ändrades eller att jämföra mappstorlekar.

## Se även

* [FileManager dokumentation](https://developer.apple.com/documentation/foundation/filemanager)
* [Swift String och Characters](https://developer.apple.com/documentation/swift/string)
* [Handling FilSystemändring Notifications i Swift](https://medium.com/@maurograssini91/handling-filesystem-changes-with-swift-d425e5b5ffa7)