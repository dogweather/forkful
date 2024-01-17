---
title:                "Stavigering av en sträng"
html_title:           "Kotlin: Stavigering av en sträng"
simple_title:         "Stavigering av en sträng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att "kapitalisera" en sträng i programmering betyder att göra om alla bokstäver till versaler, eller stora bokstäver. Detta kan göras av olika skäl, bland annat för att förbättra läsbarheten eller för att matcha en gemensam standard för strängar. 

## Hur göra:
Detta kan göras enkelt i Kotlin med hjälp av den inbyggda funktionen "capitalize()". Detta funktion tar en sträng som argument och returnerar en ny sträng med alla bokstäver som versaler. Nedan följer ett exempel på hur man kan använda denna funktion:

```Kotlin
val str = "hej världen"
val capitalizedStr = str.capitalize()

println(capitalizedStr) // Skriver ut "Hej världen"
```

## Djupdykning:
Kapitalisering av strängar har funnits sedan tidiga programmeringsspråk och har använts för att göra det möjligt att skapa "enhetslig kod". Innan strängar skulle matas in för hand, vilket kunde leda till stavfel och varierande capitalisering. Idag finns det alternativ till att använda "capitalize()" i Kotlin, till exempel att använda reguljära uttryck eller skapa en egen funktion för capitalisering. När man förstår hur en sträng är uppbyggd är det också möjligt att implementera en egen funktion för kapitalisering.

## Se även:
- Kotlin Dokumentation: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html
- Reguljära uttryck för strängmanipulation: https://www.regular-expressions.info/index.html
- Mer om strängar i Kotlin: https://kotlinlang.org/docs/reference/basic-types.html#strings