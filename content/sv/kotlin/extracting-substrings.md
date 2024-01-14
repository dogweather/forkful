---
title:                "Kotlin: Utdrag av delsträngar"
simple_title:         "Utdrag av delsträngar"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför
Det kan finnas flera olika anledningar till varför man vill extrahera substrängar i Kotlin. Det kan vara för att få ut specifikt data från en sträng, manipulera data eller bara för att göra koden mer läsbar. Oavsett anledning kan förmågan att extrahera substrängar vara mycket användbar i Kotlin.

## Hur man gör det
För att extrahera substrängar i Kotlin använder vi oss av funktionen `substring()` som finns tillgänglig för alla strängobjekt. Syntaxen för denna funktion är `string.substring(startIndex, endIndex)`, där `startIndex` är indexet för den första tecknet i substrängen och `endIndex` är indexet för det sista tecknet i substrängen. Det är viktigt att notera att `endIndex` är exklusivt, vilket innebär att tecknet på detta index inte kommer att vara inkluderat i den resulterande substrängen.

Låt oss ta ett enkelt exempel för att förstå detta bättre:

```Kotlin
val str = "Hej alla där ute!"
val subStr = str.substring(4, 9)
println(subStr)
```

Output: `alla`

Vi börjar med att deklarera en strängvariabel `str` med värdet "Hej alla där ute!". Sedan använder vi funktionen `substring()` för att extrahera en ny substräng från index 4 till index 9 i `str` och tilldelar det till en ny variabel `subStr`. Slutligen skriver vi ut den nya substrängen och får som resultat "alla".

Vi kan också ange endast `startIndex`, vilket i så fall kommer att inkludera alla tecken från det indexet till slutet av strängen. Om vi om vi inte specificerar något `endIndex` kommer det att antas vara strängens slut.

```Kotlin
val str = "Det är en fin dag!"
val subStr = str.substring(12)
println(subStr)
```

Output: `fin dag!`

Här extraherar vi en substräng från index 12 till slutet av strängen och får som resultat "fin dag!".

## Djupdykning
Det finns några viktiga saker att komma ihåg när man arbetar med `substring()` i Kotlin. För det första, om `startIndex` är större än `endIndex` kommer funktionen att returnera en tom sträng. Dessutom, om antingen `startIndex` eller `endIndex` är utanför strängens indexområdet, kommer en `IndexOutOfBoundsException` att kastas.

En annan intressant funktion är `substringAfter()` och `substringBefore()`. Dessa funktioner använder sig av ett separat tecken som markör för att extrahera substrängar. Om vi till exempel har en sträng `Hej alla! Välkommen till min sida` och använder `substringAfter("!")`, kommer den att returnera en substräng från det första utropstecknet till slutet, vilket är ` Välkommen till min sida`.

## Se även
- [Kotlin String Interpolation](https://kotlinlang.org/docs/reference/basic-types.html#string-interpolation)
- [Kotlin do while() Loop](https://kotlinlang.org/docs/reference/control-flow.html#do-while-loops)
- [Kotlin Ranges](https://kotlinlang.org/docs/reference/ranges.html)