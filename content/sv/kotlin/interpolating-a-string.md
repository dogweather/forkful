---
title:                "Interpolering av en sträng"
html_title:           "Kotlin: Interpolering av en sträng"
simple_title:         "Interpolering av en sträng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När vi pratar om att interpolera en sträng i Kotlin, så menar vi att kombinera en fast text med variabler eller uttryck för att generera en dynamisk sträng. Detta kan vara användbart när vi till exempel vill skapa en text som innehåller specifika användardata eller beräkningar.

Interpolering av strängar är ett vanligt sätt för programmerare att skapa dynamiska och anpassade strängar baserat på variabler och data, istället för att hårdkoda dem. Det gör koden mer flexibel och läsbar på ett intuitivt sätt.

## Så här gör du:
Enkel interpolation i Kotlin görs genom att lägga till en "$" tecken följt av en variabel eller ett uttryck inom en sträng. Det går också att använda "{}" för att ange ett uttryck inom en sträng.

Exempel: 

```Kotlin
val name = "Jenny"
val profession = "programmerare"

println("Hej, mitt namn är $name och jag är en $profession.")
```

Output: "Hej, mitt namn är Jenny och jag är en programmerare."

Ibland behöver vi också formatera utdata, till exempel för att visa ett decimaltal med enbart två decimaler. Detta kan enkelt åstadkommas med hjälp av stränginterpolation.

Exempel:

```Kotlin
val pi = 3.14159265359

println("Pi är ca ${"%.2f".format(pi)}")
```

Output: "Pi är ca 3.14"

## Djup Dykning:
Interpolering av strängar är inte unikt för Kotlin, utan har funnits i andra programmeringsspråk som Ruby och Python sedan länge. I Java, som Kotlin är baserat på, har det länge varit möjligt att interpolera strängar med hjälp av tredje parts bibliotek.

En alternativ metod för att skapa dynamiska strängar är att använda en StringBuilder. Detta är dock betydligt mer omständligt och mindre läsbart jämfört med interpolering.

När det gäller implementation i Kotlin, så konverteras stränginterpolation av kompilatorn till StringBuilder-kod under huven. Detta innebär att det inte finns någon prestandaförlust när man använder det.

## Se även:
- [Kotlin Language Reference - String Interpolation](https://kotlinlang.org/docs/reference/basic-types.html#string-interpolation)
- [Java String.format() exempel](https://www.baeldung.com/java-string-format) för att forma utdata med hjälp av tredje parts bibliotek i Java.