---
title:    "Kotlin: Skriving til standardfeil"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standardfeil (standard error) er en viktig del av programmering i Kotlin. Når du bruker standardfeil, har du mulighet til å håndtere og kommunisere om eventuelle feil eller unntak som kan oppstå i programmet ditt. Dette hjelper deg med å identifisere og feilsøke problemer for å sikre at programmet ditt fungerer som forventet.

## Hvordan

For å skrive til standardfeil i Kotlin, kan du bruke "printError" -funksjonen fra standardbiblioteket. Denne funksjonen tar inn en streng som argument og skriver den ut til standardfeilen.

```
Kotlin

fun main(args: Array<String>) {
    printError("En feil har oppstått!") // Kaller funksjonen og skriver ut en feilmelding
}
```

Når du kjører dette programmet, vil utdataen se slik ut i din konsoll:

```
En feil har oppstått!
```

Du kan også kombinere standardfeil med try-catch-blokker for å håndtere feil i koden din. Her er et eksempel:

```
Kotlin

fun main(args: Array<String>) {
    try {
        val num = "dette er ikke et tall".toInt() // prøver å konvertere en streng til et tall
        println(num) // Vil aldri bli utskrevet på grunn av en NumberFormatException
    } catch (e: NumberFormatException) { // Håndterer unntaket og skriver ut feilmeldingen til standardfeil
        printError("Kunne ikke konvertere til et tall!")
    }
}
```

Når du kjører dette programmet, vil utdataen se slik ut:

```
Kunne ikke konvertere til et tall!
```

## Dypdykk

Det er viktig å merke seg at skriving til standardfeil ikke påvirker utførelsen av programmet ditt. Det vil fortsatt kjøre og fullføre, selv om det skrives til standardfeil. Dette er nyttig når du ønsker å håndtere feil uten å stoppe programmet.

Det finnes også andre måter å skrive til standardfeil på, som å bruke "System.err" -objektet eller bruke "PrintWriter" -klassen fra Java biblioteket. Hvilken metode du velger å bruke, avhenger av hva som passer best for programmet ditt.

## Se også

- [Kotlin Standard Library Reference for printError function](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/print-error.html)
- [Try-catch i Kotlin](https://kotlinlang.org/docs/reference/exceptions.html#try---catch---finally)