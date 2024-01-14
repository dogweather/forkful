---
title:                "Kotlin: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av å være en effektiv programmerer. Det hjelper deg å sikre at koden din fungerer som den skal og reduserer bugs og feil i produksjon.

## Hvordan

For å skrive tester i Kotlin, må du først importere JUnit-biblioteket. Deretter kan du lage en testklasse ved å legge til "@RunWith (JUnit4 :: class)" over klassenavnet og "@Test" over testmetodene.

```Kotlin
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(JUnit4::class)
class TestClass {

    @Test
    fun testMethod(){
        // test kode her
    }
}
```

Innenfor testmetoden kan du bruke ulike asserter for å sjekke at resultatene er som forventet. For eksempel kan du bruke "assertEquals" for å sammenligne to verdier og "assertTrue" for å sjekke om en betingelse er sann.

```Kotlin
@Test
fun testMethod(){
    val num1 = 5
    val num2 = 10

    assertEquals(15, num1 + num2)
    assertTrue(num1 < num2)
}
```

Du kan også bruke "assertEquals" for å sammenligne utskrift fra metoder med forventede resultater.

```Kotlin
@Test
fun testMethod(){
    val output = methodToTest()
    assertEquals("the expected output", output)
}
```

## Deep Dive

Når du skriver tester, er det viktig å tenke på forskjellige scenarier og ikke bare teste positivt. Du bør også teste for feilinndata og håndtering av unntak.

Det er også viktig å organisere testene dine på en logisk måte, kanskje ved å ha en testklasse for hver klasse du tester.

Husk også å oppdatere og vedlikeholde testene mens du gjør endringer i koden. Dette sikrer at testene er i samsvar med den oppdaterte koden.

## Se også

- [JUnit dokumentasjon](https://junit.org/junit5/docs/current/user-guide/)
- [Kotlin Testing dokumentasjon](https://kotlinlang.org/docs/reference/testing.html)
- [TDD (Test Driven Development) guide på norsk](https://www.tdd-guide.com/)