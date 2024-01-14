---
title:                "Swift: Utvinna delsträngar"
simple_title:         "Utvinna delsträngar"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför
Att extrahera substrängar är en vanlig uppgift inom Swift-programmering. Det kan vara användbart när du behöver arbeta med specifika delar av en sträng istället för hela strängen. Genom att lära dig hur man extraherar substrängar kan du bli mer effektiv i ditt kodande.

## Hur man gör det
För att extrahera en substräng från en stäng i Swift, används funktionen `substring`. Till exempel om vi har en sträng som heter `var text = "Hej Sverige"`, och vi vill extrahera ordet "Sverige", kan vi använda följande kod:

```Swift
let text = "Hej Sverige"
let startIndex = text.index(text.startIndex, offsetBy: 4) // Index där ordet "Sverige" börjar
let endIndex = text.index(text.endIndex, offsetBy: -1) // Index där ordet "Sverige" slutar
let substring = text[startIndex...endIndex] // "Sverige"
```

I det här exemplet använder vi `startIndex` och `endIndex` för att hitta rätt index för att extrahera substrängen. Vi väljer sedan ut de tecken som vi vill inkludera i substrängen med hjälp av indexen och sparar det i en ny variabel `substring`.

Vi kan också använda funktionen `prefix` och `suffix` för att extrahera en del av en sträng från början eller slutet. Till exempel:

```Swift
let text = "Hej Sverige"
let prefix = text.prefix(3) // "Hej"
let suffix = text.suffix(7) // "Sverige"
```

Som du kan se är det väldigt enkelt att extrahera substrängar i Swift, och det finns många olika sätt att göra det på beroende på dina behov.

## Djupdykning
Det finns fler funktioner som kan vara användbara för att extrahera substrängar i Swift, till exempel `range` som kan användas för att hitta en viss del av en stäng och returnera dess index. Det finns också möjlighet att använda funktioner som `dropFirst` och `dropLast` för att ta bort delar av en sträng och sedan använda kvarvarande delar som substrängar.

Det är också viktigt att vara medveten om hur `index` och `offset` fungerar för att undvika att få ut felaktiga substrängar. Det kan vara bra att öva och experimentera med olika funktioner för att få en bättre förståelse för hur de fungerar.

## Se även
- [Dokumentation för `String` klassen i Swift](https://developer.apple.com/documentation/swift/string)
- [Generell information om substrängar i Swift](https://www.hackingwithswift.com/example-code/strings/how-to-extract-a-substring-from-a-string)
- [Mer information om `index` och `offset` i Swift](https://www.hackingwithswift.com/articles/78/how-to-use-index-and-offset-in-swift)