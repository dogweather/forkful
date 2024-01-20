---
title:                "Gjøre en streng stor"
html_title:           "Kotlin: Gjøre en streng stor"
simple_title:         "Gjøre en streng stor"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sette stor bokstav i en streng betyr å endre en streng slik at det første tegnet blir en stor bokstav. Dette er spesielt nyttig når vi trenger å formidle formelt innhold eller formatere data for visning.

## Hvordan du:
Her er noen eksempler på hvordan du kan endre øverste bokstav i en streng i Kotlin:

```Kotlin
fun main() {
  val minStreng = "olle bolle"
  val storBokstav = minStreng.capitalize()
  println(storBokstav)
}
```

Koden ovenfor vil gi følgende utskrift:
```Kotlin
Olle bolle
```

## Dypdykk
Historisk, programmeringsspråk har lenge hatt funksjoner for å behandle strengeverdier, inkludert kapitalisering. Kotlin tar det et skritt videre ved å gi oss innebygde funksjoner som `capitalize()`.

Det er andre måter å kapitalisere en streng på i Kotlin. For eksempel, du kan bruke `substring()`. Følgende demostrerer hvordan du bruker `substring()` til å kapitalisere en streng:

```Kotlin
fun main() {
  val minStreng = "olle bolle"
  val storBokstav = minStreng[0].uppercase() + minStreng.substring(1)
  println(storBokstav)
}
```
Dette vil også generere utskriften:
```Kotlin
Olle bolle
```
Observer at `capitalize()` -funksjonen tar hensyn til regex og andre språkskapende variasjoner, mens `substring()` kun vil kapitalisere første tegn i strengen.

## Se Også
For ytterligere lesing, sjekk ut disse linkene:

- [Kotlin, Capitalizing Strings](https://stackoverflow.com/questions/50549973/kotlin-capitalize-method-is-deprecated-what-is-the-alternative)
- [Understanding Kotlin’s capitalization functions](https://proandroiddev.com/understanding-kotlins-capitalization-functions-5c4fee13e31e)