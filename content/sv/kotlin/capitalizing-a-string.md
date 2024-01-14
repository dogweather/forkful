---
title:                "Kotlin: Kapitalisering av en sträng"
simple_title:         "Kapitalisering av en sträng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna förstå grunderna i Kotlin-programmering är viktigt för alla som vill bli framgångsrika programmerare. En viktig del av detta är att kunna manipulera strängar, inklusive att första bokstaven i en sträng blir stor bokstav. Detta kan verka som en liten detalj men det är en viktig del av att skriva läsbar och korrekt kod.

## Hur man gör

För att första bokstaven i en sträng ska bli stor bokstav, kan du använda funktionen `capitalize()` i Kotlin. Du kan använda denna funktion på en enskild sträng eller på en lista av strängar. Här är ett exempel:

```Kotlin
val namn = "sara"
println(namn.capitalize()) // output: Sara
```

Du kan också använda `capitalize()` på en lista av strängar, vilket gör att första bokstaven i varje sträng blir stor bokstav. Här är ett exempel som visar hur man kan använda `map()` funktionen för att tillämpa `capitalize()` på en lista av strängar:

```Kotlin
val namnLista = listOf("anna", "peter", "måns")
val namnMedStoraBokstäver = namnLista.map { it.capitalize() }
println(namnMedStoraBokstäver) // output: [Anna, Peter, Måns]
```

Det finns också en annan liknande funktion kallad `decapitalize()` som omvandlar den första bokstaven till en liten bokstav. Detta kan vara användbart om du behöver standardisera strängar i ditt program.

## Djupdykning

Förutom `capitalize()` och `decapitalize()` finns det också andra sätt att manipulera strängar i Kotlin. En viktig funktion är `substring()`, som låter dig välja en del av en sträng baserat på start- och slutindex. Här är ett exempel:

```Kotlin
val sträng = "hej javaprogrammerare"
println(sträng.substring(4,12)) // output: javaprogr
```

En annan användbar funktion är `toLowerCase()` och `toUpperCase()` som låter dig ändra bokstäver till antingen små eller stora. Dessa funktioner kan vara användbara för att jämföra strängar utan att behöva bekymra dig om skillnader i stora och små bokstäver.

## Se också

- [Officiell Kotlin hemsida](https://kotlinlang.org)
- [Kotlin GitHub repository](https://github.com/JetBrains/kotlin)
- [Kotlin Playground](https://play.kotlinlang.org)
- [Kotlin Standard Bibliotek](https://kotlinlang.org/api/latest/jvm/stdlib/index.html)