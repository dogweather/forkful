---
title:                "Sökning och ersättning av text"
html_title:           "Kotlin: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Textsökning och ersättning är en process där man söker efter ett visst mönster i en text och ersätter det med ett annat. Detta är en viktig del av programmering eftersom det hjälper till att effektivisera och automatisera uppgifter som annars skulle ta lång tid att göra manuellt.

## Hur man gör:
Det finns flera sätt att söka och ersätta text i Kotlin, men det enklaste sättet är att använda inbyggda funktioner i standardbiblioteket.

```
val text = "Hej vad heter du?"
val nyText = text.replace("hej", "Hallå") // nyText blir "Hallå vad heter du?"
```

Man kan också använda reguljära uttryck för att söka efter mer komplexa mönster.

```
val text = "Min favoritfärg är blå, men jag gillar också grönt."
val nyText = text.replace(Regex("[blågrön]"), "röd") // nyText blir "Min favoritfärg är röd, men jag gillar också röd."
```

## Djupdykning:
Textsökning och ersättning är en process som har funnits sedan början av datavetenskapen. Innan datorer fanns det speciella maskiner som programmerades för att söka och ersätta text. Idag finns det många olika program och verktyg som hjälper till med detta, men det är fortfarande en viktig del av programmering.

Det finns också alternativ till inbyggda funktioner för textsökning och ersättning i Kotlin. Ett populärt verktyg är Regex, som låter dig söka efter mönster i en text med hjälp av reguljära uttryck.

När det kommer till implementation används ofta algoritmer som Boyer-Moore och Knuth-Morris-Pratt för att effektivt söka efter mönster i en text.

## Se även:
- [Kotlin Standardbibliotek](https://kotlinlang.org/api/latest/jvm/stdlib/)
- [Regex i Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/index.html)