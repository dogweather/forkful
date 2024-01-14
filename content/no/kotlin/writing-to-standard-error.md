---
title:                "Kotlin: Skriver til standard feil."
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standardfeil er en viktig del av programmering som ofte blir oversett. Det kan virke som en enkel og ubetydelig del av koding, men det kan faktisk være veldig nyttig i feilhåndtering og debugging av kode.

## Hvordan

For å skrive til standardfeil i Kotlin, bruker du `System.err.println()`. Dette vil skrive ut en melding til standardfeilen, som vanligvis vises på konsollen eller loggfilen.

```Kotlin
fun main() {
    System.err.println("En feil har oppstått!")
}
```

Output:

`En feil har oppstått!`

Du kan også bruke `System.err.print()` hvis du bare ønsker å skrive ut en enkel melding uten linjeskift.

```Kotlin
fun main() {
    System.err.print("Dette er en melding ")
    System.err.print("uten linjeskift.")
}
```

Output:

`Dette er en melding uten linjeskift.`

En annen måte å skrive til standardfeil er ved å bruke `e.printStackTrace()` i en try-catch blokk. Dette vil skrive ut hele stakken av feilmeldinger som kan være nyttig for debugging av koden din.

```Kotlin
fun main() {
    try {
        val num = 10 / 0
    } catch (e: ArithmeticException) {
        e.printStackTrace()
    }
}
```

Output:

```
java.lang.ArithmeticException: / by zero
	at MainKt.main(main.kt:3)
```

## Dypdykk

Å skrive til standardfeil er spesielt nyttig når du feilhåndterer og ønsker å gi mer informasjon om hva som gikk galt. I tillegg kan det hjelpe til med å finne feil i koden din ved å vise stakksporingen. Det er også en praktisk måte å logge informasjon i produksjonsmiljøet ditt.

En ting å huske på er at når du skriver til standardfeil, vil meldingen bli skrevet ut uavhengig av om koden din kjører i interaktivt modus eller ikke. Derfor kan det være lurt å bruke `System.err` kun i produksjonskoden din, og heller bruke `System.out` for å skrive til standardutgang under testing og debugging.

## Se også

* [Offisiell Kotlin dokumentasjon om System klassen](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.system/index.html)
* [Artikkel om feilhåndtering i Kotlin](https://blog.kotlin-academy.com/kotlin-exception-handling-101-79e928f5a9e)
* [Eksempelkode for feilhåndtering i Kotlin](https://github.com/MindorksOpenSource/Kotlin-exception-handling-example)