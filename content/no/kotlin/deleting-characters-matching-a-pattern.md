---
title:                "Kotlin: Sletting av tegn som samsvarer med et mønster"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange grunner til å slette tegn som matcher et mønster i programmering. Kanskje du ønsker å rydde opp i unødvendig data, eller kanskje du vil filtrere ut uønskede tegn fra en tekststreng. Uansett hva årsaken er, kan du enkelt gjøre dette ved hjelp av Kotlin-programmering.

## Hvordan du gjør det

For å slette tegn som matcher et mønster, kan du bruke Kotlin's `replace()`-metode sammen med regulære uttrykk.

```Kotlin
val regex = "[aeiou]".toRegex()
val tekst = "Dette er en tekststreng med noen uønskede vokaler"

val nyTekst = tekst.replace(regex, "") // Resultatet vil være "Dtt r n txtstrng m nn nskd vklr"
```

Her har vi definert et regulært uttrykk som matcher alle vokaler, og deretter brukt `replace()`-metoden til å erstatte disse tegnene med en tom streng. Dette vil resultere i en ny tekststreng uten de uønskede vokalene.

Du kan også spesifisere et spesifikt antall tegn som skal slettes ved å bruke `{n}`, der `n` er antall tegn du ønsker å fjerne.

```Kotlin
val regex = "[0-9]{2}".toRegex()
val tallrekke = "K1o2t3l4i5n"

val nyRekke = tallrekke.replace(regex, "") // Resultatet vil være "Kotlin"
```

I dette eksempelet har vi brukt et regulært uttrykk som matcher to tall i rekken, og deretter erstattet dem med en tom streng. Dette vil resultere i en ny tekststreng med bare bokstavene "Kotlin".

## Dypdykk

Ved hjelp av Kotlin kan du også slette tegn basert på deres posisjon i en tekststreng. Dette kan være nyttig hvis du for eksempel ønsker å fjerne de første eller siste tegnene i en streng.

```Kotlin
val tekst = "Dette er en tekststreng"

val nyTekst1 = tekst.drop(6) // Vil fjerne de første 6 tegnene og gi "er en tekststreng"
val nyTekst2 = tekst.dropLast(9) // Vil fjerne de siste 9 tegnene og gi "Dette er en tekst"
```

Her har vi brukt Kotlin's `drop()`-metode til å fjerne et spesifikt antall tegn fra starten eller slutten av en tekststreng. Dette er spesielt nyttig hvis du vil fjerne for eksempel HTML-koder eller lignende som alltid er på samme posisjon i strengen.

## Se også

* [Kotlin - Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
* [Java Regular Expressions - Pattern](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html)
* [Kotlin - Regular Expressions](https://kotlinlang.org/docs/reference/regular-expressions.html)