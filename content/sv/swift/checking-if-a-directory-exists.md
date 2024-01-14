---
title:                "Swift: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

I denna bloggpost kommer vi att utforska hur man kontrollerar om en katalog finns i Swift.

Att kunna undersöka om en katalog existerar är en viktig del av programmering. Det kan hjälpa dig att undvika fel i din kod och säkerställa att allt fungerar som det ska.

## Hur man gör

För att kontrollera om en katalog existerar i Swift, kan du använda funktionen `FileManager.default.fileExists(atPath: <path>)`. Låt oss titta på ett exempel:

```Swift
// Skapar en URL för katalogen vi vill kontrollera
let directoryURL = URL(fileURLWithPath: "/Users/min användare/Documents/Projekt")

// Använder funktionen för att se om katalogen existerar
if FileManager.default.fileExists(atPath: directoryURL.path) {
    print("Katalogen finns!")
} else {
    print("Katalogen finns inte :(")
}
```

I detta exempel skapar vi en URL för katalogen "Projekt" i mappen "Documents" för användaren "min användare". Sedan använder vi `FileManager` för att kontrollera om katalogen existerar genom att använda `fileExists(atPath:)`-funktionen på URL:en.

Om katalogen existerar, skrivs "Katalogen finns!" ut. Om katalogen inte finns, skrivs "Katalogen finns inte :(" ut.

## Deep Dive

När man kontrollerar en katalogs existens i Swift, används funktionen `fileExists(atPath:)` egentligen för att kontrollera om filen eller katalogen på den angivna sökvägen existerar.

Om stigen som du anger pekar på en katalog, kommer `fileExists(atPath:)` att returnera `true` oavsett om katalogen är tom eller inte. Om stigen pekar på en fil, kommer funktionen att returnera `true` om filen finns och `false` om den inte finns.

Det är värt att notera att denna funktion inte kontrollerar om du har behörighet att komma åt den angivna stigen eller om det finns några skrivskyddade filer i katalogen. Det är därför viktigt att vara uppmärksam på eventuella fel som kan uppstå när du försöker komma åt en katalog eller fil.

## Se även

Här är några andra användbara resurser för att lära dig mer om fil- och kataloghantering i Swift:

- [Apple Developer Documentation - FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Swifting.io - Working with files in Swift](https://swifting.io/blog/2016/01/26/17-working-with-files-in-swift/)

Tack för att du läste! Vi hoppas att denna bloggpost varit hjälpsam för dig att lära dig hur man kontrollerar om en katalog existerar i Swift. Kom ihåg att alltid vara uppmärksam på eventuella fel eller problem som kan uppstå när du hanterar filer och kataloger i din kod. Ha det kul med Swift-programmering!