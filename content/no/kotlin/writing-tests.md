---
title:                "Kotlin: Skrive tester"
programming_language: "Kotlin"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester kan virke som en ekstra byrde når man allerede har mye kode å skrive, men det er en viktig praksis som kan hjelpe deg å lage bedre og mer pålitelig kode på lang sikt.

## Hvordan

For å skrive tester i Kotlin, kan du bruke et rammeverk som JUnit eller Spek. La oss se på et enkelt eksempel ved hjelp av JUnit:

```Kotlin
class CalculatorTest {

    private val calculator = Calculator()

    @Test
    fun `adds two numbers`() {
        val result = calculator.add(2, 3)
        assertEquals(5, result)
    }

    @Test
    fun `subtracts two numbers`() {
        val result = calculator.subtract(5, 2)
        assertEquals(3, result)
    }
}
```

Her har vi en enkel testklasse som tester to funksjoner i en kalkulator som regner ut sum og differanse mellom to tall. Vi kan se at testmetodene er annotert med ```@Test``` for å indikere at de er tester, og vi bruker en assert-funksjon for å sjekke om resultatet er som forventet.

Når vi kjører disse testene, vil vi få output som viser om testene har gått gjennom eller om det er noe som har feilet. Dette gjør det enkelt å oppdage og fikse eventuelle bugs i koden.

## Deep Dive

Skrive tester kan også hjelpe deg å bedre forstå koden din. Ved å tenke på hva som skal testes, kan du oppdage områder av koden din som kanskje er mer komplisert enn de trenger å være. Testene kan også fungere som dokumentasjon for koden din og hjelpe nye utviklere å forstå hvordan koden fungerer.

Det er også viktig å merke seg at selv om du har en godt testet kodebase, må du fortsatt fortsette å skrive og vedlikeholde tester for å sikre at koden forblir pålitelig i fremtiden.

## Se også

- [Introduksjon til JUnit](https://junit.org/junit5/docs/current/user-guide/)
- [Spek dokumentasjon](https://spekframework.org/docs/latest/)
- [Kotlin Testing](https://kotlinlang.org/docs/testing.html)