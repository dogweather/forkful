---
title:                "Swift: Hitta längden på en sträng"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

##Varför
Att hitta längden på en sträng är en grundläggande och viktig del av Swift programmering. Det tillåter dig att beräkna och manipulera textbaserade data i dina applikationer. Oavsett om du bygger en enkel kalkylator eller en avancerad mobilapplikation, är det viktigt att förstå hur man hittar längden på en sträng i Swift.

##Hur man gör
För att hitta längden på en sträng i Swift, använd "count" metod på strängen. Till exempel:

```Swift
let minSträng = "Hej, världen!"
print(minSträng.count)
```

Det här kodavsnittet kommer att skriva ut antalet tecken i strängen, vilket i detta fall är 13. Det är viktigt att notera att mellanslag och specialtecken också räknas som tecken i en sträng.

##Djupdykning
Förutom att använda "count" metod kan du också få längden på en sträng genom att använda "characters" egendom för att få en array av tecken i strängen och sedan räkna antalet element i arrayen. Detta är särskilt användbart om du vill göra mer komplexa operationer på strängen, såsom att konvertera den till en array av bokstäver för att sedan bearbeta varje bokstav separat.

##Se även
- [Official Swift String documentation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift String Cheat Sheet](https://www.raywenderlich.com/ios/regular-expressions-swift-cheat-sheet)