---
title:                "Söka och ersätta text"
html_title:           "Swift: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Sökning och ersättning av text är en vanlig uppgift för programmerare. Det handlar om att hitta specifika delar av en text och ersätta dem med annan text. Detta kan hjälpa till att snabbt göra ändringar i en stor mängd kod eller göra texten mer enhetlig.

## Så här gör du:
För att söka och ersätta text med Swift kan du använda metoden `replacingOccurrences(of:with:)`. Till exempel kan du byta ut alla förekomster av ordet "Swift" med "Kotlin" i en sträng genom att skriva:
```Swift
let text = "Jag älskar Swift!"
let newText = text.replacingOccurrences(of: "Swift", with: "Kotlin")
print(newText)
```
Resultatet blir "Jag älskar Kotlin!". Du kan också använda Regex (regular expressions) för att göra mer avancerade sökningar och ersättningar.

## Djupdykning:
Sökning och ersättning har funnits i programmering sedan tidiga datorer. Det är också en vanlig funktion i olika textredigeringsprogram. Om du inte använder Swift kan du hitta liknande metoder i andra programmeringsspråk som Java (`replace()`), Python (`replace()`), och C# (`Replace()`). Det finns också integrerade verktyg för sökning och ersättning i utvecklingsmiljöer som Xcode och Visual Studio.

## Se även:
- [Apple Swift - Working with Strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Regular-Expressions.info - Swift Regex Tutorial](https://www.regular-expressions.info/swift.html)