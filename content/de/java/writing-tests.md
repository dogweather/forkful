---
title:                "Tests schreiben"
date:                  2024-01-19
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?

Tests schreiben heißt, Code zu erstellen, der deinen Code überprüft. Programmierer machen das, um Fehler zu vermeiden, Qualität zu sichern und die Wartung zu erleichtern.

## Wie geht's:

JUnit ist eine beliebte Testbibliothek in Java. Hier siehst du ein einfaches Beispiel:

```Java
import static org.junit.Assert.*;
import org.junit.Test;

public class CalculatorTest {

    @Test
    public void testAddition() {
        Calculator calculator = new Calculator();
        assertEquals(5, calculator.add(2, 3));
    }
}

class Calculator {
    public int add(int a, int b) {
        return a + b;
    }
}
```

Output:

```
OK (1 test)
```

## Tiefgang:

Tests im Java-Ökosystem begannen mit JUnit, heute existiert eine Vielfalt: TestNG, Spock, und andere. JUnit 5 ist die aktuelle Hauptversion, die mit Extensions, Lambdas und Annotations arbeitet und für Java 8 und darüber optimiert wurde. In Sachen Implementierung beachten: saubere Testumgebungen nutzen, Abhängigkeiten minimieren und Mock-Objekte einsetzen, um Isolation und Fokussierung der Tests zu erreichen.

## Siehe auch:

- JUnit 5 User Guide: [https://junit.org/junit5/docs/current/user-guide/](https://junit.org/junit5/docs/current/user-guide/)
- Oracle's Java Tutorials – Testing: [https://docs.oracle.com/javase/tutorial/junit/index.html](https://docs.oracle.com/javase/tutorial/junit/index.html)
- Mocking frameworks: Mockito ([https://site.mockito.org/](https://site.mockito.org/))
