---
date: 2024-01-20 17:47:57.610845-07:00
description: "Att hitta l\xE4ngden p\xE5 en str\xE4ng inneb\xE4r att r\xE4kna antalet\
  \ tecken den inneh\xE5ller. Programmerare g\xF6r detta f\xF6r att validera input,\
  \ skapa loopar, eller\u2026"
lastmod: '2024-03-13T22:44:37.862547-06:00'
model: gpt-4-1106-preview
summary: "Att hitta l\xE4ngden p\xE5 en str\xE4ng inneb\xE4r att r\xE4kna antalet\
  \ tecken den inneh\xE5ller."
title: "Hitta l\xE4ngden p\xE5 en str\xE4ng"
weight: 7
---

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
