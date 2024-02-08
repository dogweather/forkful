---
title:                "Hitta längden på en sträng"
date:                  2024-01-20T17:47:57.610845-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hitta längden på en sträng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hitta längden på en sträng innebär att räkna antalet tecken den innehåller. Programmerare gör detta för att validera input, skapa loopar, eller hantera textdata effektivt.

## Hur gör man:
Kotlin gör det enkelt med `length`-egenskapen. Här är ett exempel:

```kotlin
fun main() {
    val greeting = "Hej, världen!"
    println("Stränglängd: ${greeting.length}")
}
```

Utskriften blir:

```
Stränglängd: 13
```

Du kan också hantera strängar med svenska tecken korrekt:

```kotlin
fun main() {
    val swedishWord = "Räksmörgås"
    println("Stränglängd: ${swedishWord.length}")
}
```

Utskriften blir:

```
Stränglängd: 11
```

Observera att `length` returnerar det totala antalet tecken, inklusive svenska åäö.

## Djupdykning
Historiskt sett har stränglängden varit ett grundläggande attribut i de flesta programmeringsspråk för att hantera text. I Kotlin är `length` en lättillgänglig egenskap av `String`-klassen. 

Alternativ till `length` kan inkludera manuella loopar för att räkna tecken, men det är onödigt komplicerat i Kotlin.

Det är värt att notera att `length` ger antalet `Char`-enheter i strängen. När det kommer till Unicodekodpunkter, särskilt emoji eller andra tecken som kan representeras av flera `Char`, kan `length` ge ett missvisande värde. För dessa situationer finns `codePointCount`, men i de flesta fall är `length` tillräcklig och mer prestandavänlig.

## Se också
- Kotlin officiella dokumentation för `String`: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- Unicode och dess inverkan på strängar: [https://unicode.org/](https://unicode.org/)
- Artikel om stränghantering i Kotlin: [https://kotlinlang.org/docs/strings.html](https://kotlinlang.org/docs/strings.html)
