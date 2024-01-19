---
title:                "Söka och ersätta text"
html_title:           "Bash: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad och varför?
Sökning och ersättning av text är en filbehandlingsteknik som används för att identifiera och byta ut specifika strängar i en text. Programmerare brukar använda den för debuggning, kodrefaktorisering eller attitydanpassning av data. 

## Hur du gör:
Här kommer ett exempel på hur man söker och ersätter text i Swift:

```Swift
var text = "Hej världen!"
text = text.replacingOccurrences(of: "världen", with: "Swift")
print(text) 
```
Utskriften kommer att vara: "Hej Swift!".

## Fördjupning
1. Historisk Kontext: Sökning och ersättning av text har varit en standardfunktion i textredigerare sedan tidigt 70-tal. Swift språket gör det enklare och mer intuitivt.
2. Alternativ: Det finns andra sätt att ersätta text i Swift, som att använda "range(of:)" eller regular expressions, men "replacingOccurrences(of:)" är det mest strömlinjeformade sättet.
3. Implementeringsdetaljer: "replacingOccurrences(of:)" -metoden kommer från Swifts utökning av NSString-klassen. Den letar igenom hela strängen, så tänk på prestanda vid arbete med stora textfiler.

## Se även
1. Apple Developer Documentation: [replacingOccurrences(of:with:)](https://developer.apple.com/documentation/foundation/nsstring/1416398-replacingoccurrences)
2. Swift by Sundell: [Working with strings in Swift](https://www.swiftbysundell.com/basics/strings/)
3. Hacking with Swift: [How to replace occurrences of a string within a string](https://www.hackingwithswift.com/example-code/strings/how-to-replace-occurrences-of-a-string-within-a-string)