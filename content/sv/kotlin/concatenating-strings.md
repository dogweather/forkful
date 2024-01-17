---
title:                "Sammanfogning av strängar"
html_title:           "Kotlin: Sammanfogning av strängar"
simple_title:         "Sammanfogning av strängar"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Strängkonkatenering är när man slår ihop flera olika strängar till en långare sträng. Detta kan vara användbart när man vill skapa nya strängar utifrån befintliga. Programmerare använder sig ofta av detta för att skapa användarvänliga och dynamiska gränssnitt, eller för att skapa läsbar och organiserad kod.

## Hur man gör:
```Kotlin
val name = "Anna"
val lastName = "Johansson"
val fullName = name + " " + lastName
println(fullName)
```
Output: Anna Johansson

För att konkatenera strängar i Kotlin använder man operatören "+" för att enkelt slå ihop strängarna. Ett annat sätt är genom att använda en interpolationssträng, som låter dig inkludera variabler direkt i en sträng:
```Kotlin
val age = 25
println("Jag är $age år gammal.")
```
Output: Jag är 25 år gammal.

## Fördjupning:
Strängkonkatenering är en viktig del av stränghantering i programmering, och har funnits sedan de tidiga dagarna av programmering. I vissa språk används en specialtecken för att visa strängkonkatenering, som "+" i Java och Python, eller "&" i C#. Jämfört med dessa språk erbjuder Kotlin en mer smidig och intuitiv syntax för att konkatenera strängar.

Vid användning av en stor mängd strängkonkatenering, kan det vara mer effektivt att använda sig av en StringBuilder istället för att uppdatera en variabel enskilt. Detta leder till snabbare prestanda och minskad minnesanvändning.

## Se även:
- [Kotlin Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Alternatives to String Concatenation](https://www.baeldung.com/java-string-concatenation)