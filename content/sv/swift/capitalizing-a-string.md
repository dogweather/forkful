---
title:                "Gör om en sträng till versaler"
html_title:           "Swift: Gör om en sträng till versaler"
simple_title:         "Gör om en sträng till versaler"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att förstora en sträng innebär att konvertera alla bokstäver i en text till stora bokstäver. Programmerare gör detta för att standardisera inmatning och för att göra text jämförbar oavsett format.

## Så här gör du:

I Swift, vi kan förstora en sträng genom att använda `uppercased()` funktion.

```Swift
let sentence = "hej världen"
let capitalizedSentence = sentence.uppercased()
print(capitalizedSentence)
```

Detta kommer att producera: 

```Swift
"HEJ VÄRLDEN"
```

## Fördjupning:

Historiskt sett används förstoring av strängar för att ta bort inkonsekvenser vid jämförelse av strängar. Alternativt, om du bara vill förstora den första bokstaven i varje ord, kan du använda `capitalized` egenskap.

```Swift
let sentence = "hej världen"
let capitalizedSentence = sentence.capitalized
print(capitalizedSentence)
```

Och resultatet blir:

```Swift
"Hej Världen"
```

Förstoring av en sträng i Swift använder Unicode-skalningar, vilket gör att den fungerar med alla språk som stöds av Unicode, inte bara engelska.

## Se även:

För mer information om strängmanipulation i Swift, se dokumentationen: [Apple Swift String Documentation](https://developer.apple.com/documentation/swift/string). Om du är intresserad av att lära dig mer om Unicode, läs denna artikel: [Unicode Explained](https://www.joelonsoftware.com/2003/10/08/unicode-explained/).