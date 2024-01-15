---
title:                "Sammenslåing av strenger"
html_title:           "Kotlin: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har programmert, har du nok støtt på situasjoner der du trenger å kombinere to eller flere tekststrenger. Dette er når metoden for å sette sammen eller "concatenate" strenger blir nyttige. I denne artikkelen vil vi se nærmere på hvordan du kan bruke Kotlin for å effektivt sette sammen strenger.

## Hvordan Sette Sammen Strenger i Kotlin

Kotlin tilbyr flere måter å kombinere strenger på. La oss se på noen eksempler:

```Kotlin
val navn = "John"
val etternavn = "Doe"
println("Velkommen, $navn $etternavn")
```

Dette vil gi følgende output: `Velkommen, John Doe`. I dette eksempelet bruker vi String-templatene i Kotlin, som lar oss sette inn verdier av variabler direkte i en tekststreng ved hjelp av `$`-tegnet.

Det er også mulig å bruke metoden `plus()` for å sette sammen strenger. Dette kan gjøres på følgende måte:

```Kotlin
val sted = "Oslo"
val land = "Norge"
println(sted.plus(", ").plus(land))
```

Denne koden vil gi følgende output: `Oslo, Norge`.

## Deep Dive

Når det kommer til å sette sammen strenger, er det viktig å være oppmerksom på at Kotlin har en innebygd `StringBuilder`-klasse som er optimal for å bygge og manipulere store tekststrenger. Dette er spesielt nyttig når du har en lang tekststreng og trenger å legge til flere elementer i den.

```Kotlin
val byer = arrayOf("Oslo", "Stockholm", "København")
val sb = StringBuilder()
for (i in byer.indices) {
    sb.append(byer[i])
    if (i < byer.size - 1) {
        sb.append(", ")
    }
}
println(sb.toString())
```

I dette eksempelet bruker vi `StringBuilder` for å legge til kommaseparerte bynavn i en tekststreng. Output vil være `Oslo, Stockholm, København`. Merk at `StringBuilder`-klassen også tilbyr metoder som `insert()` og `replace()` for å utføre mer avanserte operasjoner på tekststrenger.

## Se Også

- [Offisiell Kotlin Dokumentasjon](https://kotlinlang.org/docs/reference/basic-types.html#string-templates)
- [Kotlin String Builder Dokumentasjon](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/)
- [Konkatinering av Strenger i Kotlin Tutorial](https://kotlinlang.org/docs/reference/basic-types.html#strings-and-regular-expressions)