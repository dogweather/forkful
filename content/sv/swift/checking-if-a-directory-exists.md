---
title:                "Kontrollera om en katalog finns"
html_title:           "C: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en mapp (directory) finns är en enkel men kritisk operation inom programmering. Den används för att förhindra fel, som kan uppstå om programmet försöker läsa eller skriva till en mapp som inte finns.

## Hur:
Rakt på sak, här är kodexempel i Swift för att kontrollera om en mapp finns:

```Swift
import Foundation

let fileManager = FileManager.default
let directoryPath = "/path/to/the/directory"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: directoryPath, isDirectory: &isDirectory) {
    if isDirectory.boolValue {
        print("Mappen finns!")
    } else {
        print("Filen finns, men det är inte en mapp.")
    }
} else {
    print("Mappen finns inte.")
}
```

Kör den här koden och den kommer att skriva ut om mappen finns eller inte.

## Djup Dykning
Historiskt sett har katalogcheckar alltid varit en del av operativsystem. Mer moderna språk som Swift har målet att göra dessa operationer så smärtfria som möjligt.

Alternativt kan du använda `attributesOfItem(atPath:)` metoden på `FileManager` för att utföra samma check, men det kan kasta ett undantag om filen inte finns, så du behöver hantera det.

Swifts implementation av denna check är till stor del beroende av underliggande operativsystem kall, vilket gör det både snabbt och pålitligt.

## Se Även
För ytterligare information om filsystem operationer med Swift, se den officiella dokumentation: ['Apple Developer Documentation'](https://developer.apple.com/documentation)