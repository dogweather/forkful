---
title:    "Swift: Skapa en temporär fil"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Varför
Skapa en tillfällig fil kan vara användbar i många olika situationer, särskilt när vi arbetar med data som inte behöver sparas permanent eller när vi behöver generera en unik fil som bara kommer att användas temporärt.

## Så här gör du
För att skapa en temporär fil i Swift, behöver vi först importera `Foundation` ramverket. Sedan kan vi använda `NSTemporaryDirectory()` funktionen för att få den temporära filens sökväg.

```Swift
import Foundation
let tempPath = NSTemporaryDirectory()
```

Vi kan använda `URL` klassen för att skapa en temporär fil i den angivna sökvägen.

```Swift
let tempURL = URL(fileURLWithPath: tempPath).appendingPathComponent("tempfile.txt")
```

Vi kan nu skriva data till vår temporära fil genom att använda `write()` metoden på en `String` eller `Data` objekt.

```Swift
let data = "Det här är en temporär fil.".data(using: .utf8)
try data?.write(to: tempURL)
```

Vi kan också läsa data från vår temporära fil genom att använda `contentsOf` metoden på `String` eller `Data` klassen.

```Swift
let output = try String(contentsOf: tempURL)
print(output) // "Det här är en temporär fil."
```

När vi är klara med vår temporära fil, bör vi ta bort den för att frigöra utrymme på vår enhet. Detta kan göras genom att använda `FileManager` klassen och `removeItem` metoden.

```Swift
let fileManager = FileManager.default
try fileManager.removeItem(at: tempURL)
```

## Djupdykning
När vi skapar en temporär fil med `URL` klassen, skapas faktiskt en osynlig fil på enheten. Denna fil är unik för varje användning av `NSTemporaryDirectory()` funktionen och kommer automatiskt att raderas när den inte längre behövs.

Det är också värt att notera att det finns flera sätt att skapa en temporär fil i Swift, till exempel genom att använda `tmpfile()` funktionen från C-stdlib-biblioteket eller genom att använda NSFileManager class. Men genom att använda metoden som beskrivs ovan garanterar vi att vår temporära fil kommer att raderas automatiskt när den inte längre behövs.

## Se även
- [Official Swift Documentation on Temporary Files](https://developer.apple.com/documentation/security/file_system/working_with_temporary_files)
- [Swift Language Guide - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [NSFileManager Class Reference](https://developer.apple.com/documentation/foundation/nsfilemanager)