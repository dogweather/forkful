---
title:                "Scrivere test"
html_title:           "Arduino: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Scrivere test è il processo di creare script programmati che controllano il funzionamento del codice. I programmatori lo fanno per assicurare che il codice funzioni correttamente e per prevenire la comparsa di bug nel futuro.

## Come fare:

In Kotlin puoi scrivere test unitari usando JUnit. Ecco una classe di esempio e un test unitario semplice.

```kotlin
class Calcolatrice {
    fun somma(a: Int, b: Int): Int {
        return a + b
    }
}

class CalcolatriceTest {
    @Test
    fun testSomma() {
        val calc = Calcolatrice()
        val risultato = calc.somma(2, 3)
        assertEquals(5, risultato)
    }
}
```

Output del test:

```
Test passed: testSomma(0.01s)
```

## Approfondimento

Testare codice ha radici nelle prime giornate dello sviluppo software. Alternativamente, potresti usare strumenti come Mockito per mock objects o KotlinTest per un approccio specifico per Kotlin. I test possono essere scritti a diversi livelli: unità, integrazione, sistema, accettazione. 

## Vedi Anche:

- Kotlin Testing Docs: https://kotlinlang.org/docs/reference/testing-overview.html
- JUnit 5 User Guide: https://junit.org/junit5/docs/current/user-guide/
- Mockito: https://site.mockito.org/
- KotlinTest: https://github.com/kotest/kotest
