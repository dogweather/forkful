---
title:                "Escribiendo pruebas"
html_title:           "Arduino: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/writing-tests.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir tests es crear código para probar otro código. Programadores lo hacen para asegurarse de que su código funciona como debe y para prevenir fallos en el futuro.

## Cómo Hacerlo:
Imagina que tienes una función simple para sumar dos números. Vamos a escribir un test para ella usando JUnit, que es un framework popular para tests en Java.

```java
import org.junit.jupiter.api.*;

public class CalculadoraTest {

    @Test
    public void testSuma() {
        assertEquals(5, Calculadora.suma(2, 3));
    }
}

public class Calculadora {
    public static int suma(int a, int b) {
        return a + b;
    }
}
```

Output esperado al pasar el test:

```
Test passed.
```

Si hay un fallo, veríamos algo como:

```
org.opentest4j.AssertionFailedError: expected: <5> but was: <4>
```

## Profundización:
Históricamente, los tests han estado menospreciados, pero enfoques como TDD (Test-Driven Development) los han hecho esenciales. Alternativas a JUnit incluyen TestNG y Spock. El detalle clave al escribir tests es que deben ser independientes y repetibles, sin efectos secundarios que alteren resultados futuros.

## Ver También:
- JUnit 5 User Guide: [https://junit.org/junit5/docs/current/user-guide/](https://junit.org/junit5/docs/current/user-guide/)
- Spock Framework: [http://spockframework.org](http://spockframework.org)
- Artículo sobre TDD: [https://martinfowler.com/bliki/TestDrivenDevelopment.html](https://martinfowler.com/bliki/TestDrivenDevelopment.html)