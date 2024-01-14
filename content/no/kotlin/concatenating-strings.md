---
title:    "Kotlin: Sammenføyning av tekststrenger"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Hvorfor?

Å kombinere eller "konkatener" strenger er en viktig del av programmering, spesielt når man jobber med tekstbaserte applikasjoner. Det tillater deg å bygge mer komplekse og dynamiske setninger og meldinger som er nødvendig for mange programmeringsoppgaver.

# Hvordan?

La oss ta en titt på hvordan du kan konkatener strenger i Kotlin.

For å begynne kan vi definere to strenger som vi ønsker å kombinere:

```Kotlin
val navn = "Henrik"
val yrke = "programmerer"
```

For å konkatere disse to strengene, bruker vi operatøren "+" og et mellomrom for å få en leselig streng:

```Kotlin
val setning = navn + " er en dyktig " + yrke
println(setning)

// Output: Henrik er en dyktig programmerer
```

Vi kan også bruke "$" -tegnet for å inkludere variabler direkte i en streng. Dette er spesielt nyttig hvis du trenger å inkludere numeriske verdier eller variabler av andre datatyper:

```Kotlin
val alder = 30
val beskrivelse = "$navn er $alder år gammel og jobber som $yrke"
println(beskrivelse)

// Output: Henrik er 30 år gammel og jobber som programmerer
```

Vi kan også kombinere flere strenger og variabler ved å bruke funksjonen "plus" og "&" -tegnet:

```Kotlin
val hilsen = "Hei, " plus navn
val årsoppgjør = "$beskrivelse & bor i Oslo"
println("$hilsen! $årsoppgjør")

// Output: Hei, Henrik! Henrik er 30 år gammel og jobber som programmerer og bor i Oslo
```

# Deep Dive

Når du konkatenerer strenger i Kotlin, blir det laget en ny streng-objekt. Dette betyr at det kan være lite effektivt å konkatere mange strenger sammen, spesielt hvis det gjøres i en løkke. I stedet bør du bruke funksjonen "StringBuilder" som er enklere for å bygge større strenger. Her er et eksempel på hvordan du kan bruke det:

```Kotlin
val stringBuilder = StringBuilder()
val antallPersoner = 5

stringBuilder.append("Det er ").append(antallPersoner).append(" personer i rommet.")
println(stringBuilder.toString())

// Output: Det er 5 personer i rommet.
```

Ved å bruke "StringBuilder" blir det ikke opprettet nye streng-objekter hver gang en ny del blir lagt til, noe som gjør det mer effektivt når du trenger å konkatere mange strenger sammen.

# Se også

- [Kotlin Dokumentasjon](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin String Interpolering](https://kotlinlang.org/docs/reference/basic-types.html#string-interpolation) 
- [Kotlin StringBuilder](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/)