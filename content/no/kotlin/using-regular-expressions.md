---
title:    "Kotlin: Å bruke regulære uttrykk"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Hvorfor du bør bruke regulære uttrykk i Kotlin

Hvis du er en Kotlin-utvikler som ønsker å effektivisere koden din, er det en god idé å lære deg regulære uttrykk. Disse uttrykkene, også kjent som regex, er et kraftig verktøy for å søke og manipulere tekststrenger.

## Slik bruker du regulære uttrykk i Kotlin

For å bruke regulære uttrykk i Kotlin, må du først importere Regex-klassen. Deretter kan du bruke ulike metoder og uttrykk for å søke etter spesifikke mønstre i en tekststreng.

Her er et eksempel på hvordan du kan finne og erstatte alle forekomster av et bestemt ord i en tekststreng:

```Kotlin
val regex = Regex("kotlin")
val originalTekst = "Jeg elsker å kode i Kotlin!"
val endretTekst = regex.replace(originalTekst, "Java")
println(endretTekst) // Output: Jeg elsker å kode i Java!
```

Du kan også bruke regulære uttrykk til å validere inndata fra brukere. For eksempel kan du sjekke om en e-postadresse er gyldig ved å bruke følgende kode:

```Kotlin 
fun erGyldigEpost(epost: String): Boolean {
    val regex = Regex("[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,6}")
    return regex.matches(epost)
}

erGyldigEpost("example@domain.com") // Output: true
erGyldigEpost("feil_epost@com") // Output: false
```

## Dypdykk i bruken av regulære uttrykk

Regulære uttrykk kan virke komplekse i begynnelsen, men de er verdt å lære seg da de kan effektivisere koden din betraktelig. Det finnes ulike metoder og uttrykk du kan bruke for å finne og manipulere tekststrenger i Kotlin.

En av de mest nyttige metodene er `.find()`, som lar deg finne alle forekomster av et bestemt mønster i en tekststreng. Du kan også bruke `.split()` for å dele opp en tekststreng basert på et gitt mønster.

Det finnes også ulike symboler og uttrykk du kan bruke for å bygge et regulært uttrykk. For eksempel betyr `|` at det kan være enten én eller en annen verdi, og `.` betyr at det kan være hvilken som helst verdi på dette stedet.

Det er også verdt å nevne at det finnes ulike nettsteder og verktøy du kan bruke for å teste ut regulære uttrykk og lære mer om deres bruksområder.

## Se også

- [Kotlin Regex dokumentasjon](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Regex101 - Et verktøy for å teste ut regulære uttrykk](https://regex101.com/)
- [Regular-Expressions.info - En omfattende guide til regulære uttrykk](https://www.regular-expressions.info/)