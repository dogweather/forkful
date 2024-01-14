---
title:    "Kotlin: Skriving til standard feil"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å skrive til standardfeil i Kotlin kan være en effektiv måte å rette opp i feil og debugging-problemer i koden din. Når du bruker denne metoden, kan du enkelt spore hvor og når en feil oppstår i koden din, og dermed forenkle feilkorrigeringsprosessen.

## Hvordan

For å skrive til standardfeil i Kotlin, kan du bruke "System.err.println()" funksjonen. Dette vil skrive ut en feilmelding til standardfeil-strømmen. Du kan også bruke "e.printStackTrace()" funksjonen for å skrive ut en tydeligere feilmelding, som inkluderer linjenummer og filnavn. Her er et eksempel på hvordan det kan se ut i Kotlin:

```Kotlin
fun main() {
    try {
        val num1 = 10
        val num2 = 0
        val result = num1/num2
    } catch (e: ArithmeticException) {
        System.err.println("Kan ikke dele med 0!")
        e.printStackTrace()
    }
}
```

Output:
```
Kan ikke dele med 0!
java.lang.ArithmeticException: Zero division
    at MainKt.main(main.kt:4)
```

## Dypdykk

Når du bruker "System.err.println()" i Kotlin, blir feilmeldingen skrevet ut i rødt. Dette gjør det enklere å skille feilmeldinger fra andre utskrifter i konsollen. Standard feil-strømmen er også nyttig for logging, siden den ikke vil bli påvirket av andre utskrifter eller feil i koden din.

Det er også verdt å merke seg at i Kotlin, kan du også bruke "e.message" for å få en mer beskrivende feilmelding, og "e.cause" for å få ut årsaken til feilen.

## Se også

- [Dokumentasjon for Standardfeil i Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-print-stream/err.html)
- [Dealing with Exception Handling in Kotlin](https://www.baeldung.com/kotlin-exception-handling)