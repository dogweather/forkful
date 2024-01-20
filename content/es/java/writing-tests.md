---
title:                "Pruebas de escritura"
html_title:           "Java: Pruebas de escritura"
simple_title:         "Pruebas de escritura"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/writing-tests.md"
---

{{< edit_this_page >}}

## ¡Qué y por qué?

Escribir pruebas (o tests) en Java es una forma de verificar si nuestro código funciona correctamente. Los programadores lo hacen para asegurarse de que su código hace lo que se supone que debe hacer y para evitar posibles errores.

## Cómo:

Podemos escribir pruebas utilizando la clase `JUnit` en Java. Primero, debemos agregar la dependencia de `JUnit` en nuestro proyecto. Luego, creamos una clase de prueba para cada clase que queremos probar y usamos anotaciones para declarar los métodos de prueba. Finalmente, podemos verificar si el resultado esperado es igual al resultado real utilizando aserciones.

```Java
import org.junit.Test;
import static org.junit.Assert.assertEquals;

public class TestClass {

  @Test
  public void testMethod() {
    int result = 10 + 20;
    assertEquals(30, result);
  }
}
```

## Profundizando:

Escribir pruebas automatizadas es una práctica común en el desarrollo de software. Fue popularizado por Kent Beck en su libro "Extreme Programming Explained". Además de `JUnit`, existen otras herramientas de pruebas para Java, como `TestNG` y `Mockito`. También podemos escribir pruebas unitarias, de integración y de sistema, dependiendo del nivel en el que queremos probar nuestro código.

## Vea también:

- [Tutorial de JUnit](https://www.baeldung.com/junit)