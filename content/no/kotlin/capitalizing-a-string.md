---
title:                "Stor bokstav i en streng"
html_title:           "Kotlin: Stor bokstav i en streng"
simple_title:         "Stor bokstav i en streng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvem liker ikke å se sine navn og setninger med stor forbokstav? Det kan gi en følelse av viktighet og gjøre teksten mer lesbar og presentabel. I dette avsnittet, vil vi diskutere hensikten med å kapitalisere en streng med Kotlin, og hva du kan oppnå ved å gjøre det.

## Slik gjør du det
Det er flere måter å kapitalisere en streng på med Kotlin. La oss ta en titt på noen eksempler:

```Kotlin
val str = "dette ER en streng"
println(str.capitalize()) // Vil returnere "Dette er en streng"
println(str.toUpperCase()) // Vil returnere "DETTE ER EN STRENG"
println(str.decapitalize()) // Vil returnere "dette eR en streng"
```

Som du kan se, er det flere funksjoner tilgjengelig for å endre størrelsen på teksten din. Her er noen flere eksempler:

```Kotlin
val str = "dette ER en streng"
println(str.substring(0,8).toLowerCase()) // Vil returnere "dette er"
println(str.replace("ER", "er")) // Vil returnere "dette er en streng"
println(str.trim().capitalize()) // Vil returnere "Dette er en streng"
```

Kotlin gir deg mange alternativer for å tilpasse strenger på en måte som passer for deg og din kode.

## Dykk ned i detaljene
Kotlin har også innebygde funksjoner som tillater deg å kapitalisere hver enkelt ord i en streng, eller å endre casing basert på bestemte konvensjoner (f.eks. "camelCase" eller "snake_case"). I tillegg kan du også bruke regex for å finne bestemte mønstre og endre casing på dem.

Her er et eksempel på å kapitalisere hver enkelt ord i en streng:

```Kotlin
val str = "dette ER en streng"
val words = str.split(" ")

for (word in words) {
    print(word.capitalize() + " ") // Vil printe "Dette Er En Streng"
}
```

Som du kan se, er det mange forskjellige måter å kapitalisere en streng på i Kotlin, og det er bare opp til deg å velge den som passer best for din kode og dine behov.

## Se også
- [Offisiell Kotlin dokumentasjon](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin String Cheatsheet](https://blog.kotlin-academy.com/kotlin-strings-a265a847da97)
- [Kotlin Regular Expressions](https://kotlinlang.org/docs/reference/regular-expressions.html)