---
title:                "Att hitta längden på en sträng."
html_title:           "Swift: Att hitta längden på en sträng."
simple_title:         "Att hitta längden på en sträng."
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hitta längden på en sträng är att bestämma den totala mängden tecken som utgör en sträng. Programerare behöver ofta denna information för att utföra olika operationer eller för att kontrollera om förväntad input är av rätt längd.

## Så här gör du:
För att få längden på en sträng i Swift, använd ```count``` metoden på strängen och spara resultatet i en variabel.

Exempel:
```Swift
let message = "Hej världen!"
let length = message.count // output: 13
```

## Djupdykning:
Historiskt sett har det funnits flera sätt att hitta längden på en sträng, men i Swift är ```count``` metoden den mest använda och effektiva metoden. Andra språk kan ha liknande metoder som kallas ```len``` eller ```length``` istället.

## Se även:
- [String manipulation in Swift](https://www.raywenderlich.com/9481-string-processing-in-swift)
- [Working with strings in Swift](https://learnappmaking.com/working-with-string-swift-how-to-guide/)