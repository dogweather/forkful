---
title:                "Hitta längden på en sträng"
html_title:           "Swift: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden av en sträng är en viktig del av programmering eftersom det låter dig manipulera och hantera data på ett effektivt sätt. Det är också en grundläggande färdighet som är användbar för att lösa olika problem i en mängd olika program.

## Hur man gör

För att hitta längden av en sträng i Swift, kan du använda `count` metoden. Detta gör att du enkelt kan räkna antalet tecken i en sträng oavsett dess innehåll.

```Swift
let myString = "Hej! Välkommen till Swift!"
let length = myString.count
print(length) // Output: 26
```

Du kan också använda `String` objektets `count` egenskap för att hitta längden av en sträng:

```Swift
let anotherString = "🌎 Hello, world!"
let length = anotherString.count
print(length) // Output: 15
```

Det är också värt att notera att tomma strängar har en längd på 0. Så när du använder `count` metoden på en tom sträng, kommer den att returnera 0.

## Djupdykning

När du använder `count` metoden, måste du vara medveten om att det returnerar antalet Unicode-tecken i en sträng. Detta innebär att även emoticons och andra specialtecken betraktas som enbart ett tecken, även om de kan visas som flera tecken på skärmen.

Det finns också andra metoder som är relaterade till att hitta längden av en sträng, som till exempel `utf8.count` som returnerar antalet byte som behövs för att lagra strängen i UTF-8 format. Att förstå dessa olika metoder och deras användningsområden kan vara fördelaktigt i vissa situationer.

## Se även

- [The Swift Programming Language Guide](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)
- [String Manipulation in Swift](https://medium.com/swift-india/string-manipulation-in-swift-17f19945a9d8)
- [Working with Strings in Swift](https://www.hackingwithswift.com/articles/178/working-with-strings-in-swift)