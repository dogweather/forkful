---
title:                "Gör om en sträng till versaler"
html_title:           "Kotlin: Gör om en sträng till versaler"
simple_title:         "Gör om en sträng till versaler"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att markera en sträng innebär att ändra strängens första tecken till versaler (stora bokstäver). Programmerare gör detta för att göra text mer lättläst eller för att följa specifika formateringsregler.

## Hur gör man:

I Kotlin, kan du enkelt konvertera en sträng till versaler genom att använda `capitalize()` funktionen. Här är ett exempel:

```Kotlin
val str = "hej världen"
val capitalStr = str.capitalize()
println(capitalStr) //Skriva ut: "Hej världen"
```
Den här koden tar strängen "hej världen", konverterar det första tecknet till versaler, och skriver ut det nya strängvärdet "Hej världen".

## Fördjupning

Att konvertera en sträng till versaler är en vanlig operation i programmering och dess ursprung går tillbaka till de tidiga dagarna av datorkodning. Det finns olika sätt att utföra denna operation i olika programmeringsspråk, till exempel `toUpperCase()` i Java och typer-konvertering i C++. I Kotlin implementation är `capitalize()` funktionen ganska rakt på sak - den ändrar bara utfärdat tecknet till versaler.

Observera att `capitalize()` i Kotlin returnerar en ny sträng där bara det första tecknet är versaler och alla andra tecken är oförändrade. Om strängen redan börjar med ett versaltecken, returnerar den samma sträng. 

En annan intressant detalj är att denna metod är case-insensitive, vilket betyder att den inte påverkas av om tecknen redan är i versaler eller gemener.

## Se även

För mer information om string manipulation i Kotlin, se dessa länkar:
- Officiell Kotlin dokumentation för `capitalize()` funktion: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html
- StackOverflow tråd om strängkapitalisering: https://stackoverflow.com/questions/50549955/how-to-capitalize-the-first-letter-in-kotlin