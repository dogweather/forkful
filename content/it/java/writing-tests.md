---
title:                "Scrivere test"
date:                  2024-01-19
html_title:           "Arduino: Scrivere test"
simple_title:         "Scrivere test"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere test ci permette di verificare che il codice funzioni come previsto. Farlo risparmia tempo, evita errori ed è essenziale per mantenere la qualità del software durante lo sviluppo.

## How to:
Java utilizza JUnit per i test. Ecco un esempio semplice:

```java
import org.junit.jupiter.api.*;

class CalcolatriceTest {

    private Calcolatrice calcolatrice;

    @BeforeEach
    void setUp() {
        calcolatrice = new Calcolatrice();
    }

    @Test
    void testAddizione() {
        Assertions.assertEquals(5, calcolatrice.addizione(2, 3));
    }

    // Metodo da testare
    static class Calcolatrice {
        int addizione(int a, int b) {
            return a + b;
        }
    }
}
```
Output dopo l'esecuzione del test:

```
Test passed.
```

## Deep Dive
Il testing automatico esiste da quando il software è diventato complesso. JUnit, creato da Kent Beck e Erich Gamma, è uno standard de facto in Java dal 2000. Alternative includono TestNG o framework specifici per domini come Mockito per i mock objects. Dettagli d'implementazione da considerare includono l'isolamento dei test, la copertura del codice e l'integrazione con sistemi di build automatizzati come Maven o Gradle.

## See Also
- [JUnit 5 User Guide](https://junit.org/junit5/docs/current/user-guide/)
- [Maven: Surefire Plugin](http://maven.apache.org/surefire/maven-surefire-plugin/)
- [Gradle Testing](https://docs.gradle.org/current/userguide/java_testing.html)
