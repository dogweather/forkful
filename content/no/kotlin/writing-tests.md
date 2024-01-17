---
title:                "Skriving av tester"
html_title:           "Kotlin: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive tester er en vanlig praksis blant programmerere for å teste sin egen kode og sikre at den fungerer som den skal. Tester er en viktig del av utviklingsprosessen, da de bidrar til å identifisere feil og forbedre kvaliteten på koden.

## Slik gjør du det:
For å skrive tester i Kotlin må du først importere "junit-jupiter-engine" biblioteket. Deretter kan du markere testerfunksjonen med @Test og bruke assert metoden for å sjekke om forventede resultater samsvarer med faktiske resultater. Se eksempelet nedenfor:

```Kotlin
import org.junit.jupiter.api.Assertions.assertEquals

@Test
fun addNumbersTest() {
    val result = addNumbers(2, 3)
    assertEquals(5, result)
}
```

Output:
```
org.opentest4j.AssertionFailedError: expected: <5> but was: <6>
...
```

## Dypdykk:
Skriving av tester er en praksis som har eksistert i mange år, spesielt innenfor TDD (Test Driven Development) metoden. Det finnes også alternative testrammeverk som "Mockito" og "Kotest". I Kotlin er det også mulig å bruke den innebygde "assert" metoden for å sammenligne verdier. Det er viktig å huske på at tester bør være enkle og dekke alle mulige scenarioer for å sikre at koden fungerer som forventet.

## Se også:
Her er noen nyttige ressurser for å lære mer om å skrive tester i Kotlin:
- [Offisiell dokumentasjon for Kotlin testing](https://kotlinlang.org/docs/tutorials/jvm-get-started.html)
- [En introduksjonsvideo til Kotlin testing](https://www.youtube.com/watch?v=FRVm2eEBY0s)
- [Dokumentasjon for JUnit testing i Kotlin](https://junit.org/junit5/docs/current/user-guide/#writing-tests-annotations)