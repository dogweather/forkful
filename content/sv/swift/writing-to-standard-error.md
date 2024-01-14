---
title:                "Swift: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##Varför##

När du skriver Swift-kod, är det viktigt att veta hur man använder standardfelutmatning för att felsöka och förbättra din kod. Genom att skriva till standardfelutmatning kan du se specifika felmeddelanden och veta var i koden problemet uppstår.

##Så här gör du##

För att skriva till standardfelutmatning kan du använda Swifts globala funktion "print" med parametern "to:". Som standard skrivs utskriften till standardutmatningen, men genom att ange "standardError" som parametern "to:" kan du skriva till standardfelutmatning istället.

```Swift
print("Detta är ett felmeddelande", to: &standardError)
```

När du kör koden ovan kommer utskriften att skrivas till standardfelutmatning istället för standardutmatning. Du kan också inkludera variabler eller andra värden i utskriften för att få mer specifik information.

```Swift
let num = 10
print("Värdet är \(num)", to: &standardError)
```

##Djupdykning##

Att kunna använda standardfelutmatning är ett viktigt verktyg för felsökning, särskilt när det gäller utveckling av större projekt med många olika delar. Genom att skriva till standardfelutmatning kan du snabbt hitta och korrigera fel i din kod, vilket sparar tid och frustration i långa loppet.

Att inkludera variabler och värden i utskriften gör det också enklare att förstå vad som går fel och var i koden problemet finns. Du kan också använda Swifts "assert" funktion för att skriva till standardfelutmatning när ett villkor inte uppfylls, vilket kan hjälpa dig att hitta och åtgärda problem i din kod.

##Se även##

- [Swifts officiella dokumentation om standardutmatning](https://developer.apple.com/documentation/swift/standarderror)
- [En guide till att använda Swifts "print" funktion](https://www.hackingwithswift.com/example-code/language/how-to-send-text-to-the-console-using-swift)
- [Detaljerad information om Swifts "assert" funktion](https://www.swiftbysundell.com/basics/asserting-in-swift/)