---
title:                "Swift: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng är en grundläggande operation som är nödvändig i många Swift-program. Detta kan användas för att beräkna antalet tecken i en text eller för att kontrollera om en input är för lång.

## Hur man gör det

Att hitta längden på en sträng i Swift är en enkel process som bara kräver en enda rad kod. Här är en enkel kod som visar hur man hittar längden på en sträng i Swift:

```Swift
let sträng = "Hej! Välkommen till min blogg."
print(sträng.count) // Output: 30
```

I koden ovan, används metoden `.count` för att räkna antalet tecken i en strängvariabel. Detta ger oss en output på 30, eftersom det finns totalt 30 tecken i den givna strängen.

## Djupdykning

När man använder metoden `.count` för att hitta längden på en sträng, är det viktigt att komma ihåg att den räknar alla tecken, inklusive mellanslag och specialtecken. Dessutom finns det andra sätt att hitta längden på en sträng, såsom att använda metoden `.length` för att räkna antalet Unicode-skalära värden.

Det är också viktigt att notera att metoden `.count` inte fungerar på alla typer av variabler, utan bara på strängar. Om du vill hitta längden på en annan typ av variabel, som en array, finns det andra metoder som kan användas.

## Se även

Här är några användbara resurser för att lära dig mer om att hitta längden på en sträng i Swift:

- [Swift - Working with Strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift String Cheat Sheet](https://link.medium.com/z9DrFFrAPgb)
- [Count characters in a string using Swift](https://stackoverflow.com/questions/38921173/count-characters-in-a-string-using-swift)

Lycka till med att använda detta praktiska verktyg för att hitta längden på strängar i dina Swift-program!