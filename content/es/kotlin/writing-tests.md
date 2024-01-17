---
title:                "Escribiendo pruebas"
html_title:           "Kotlin: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Escribir pruebas en programación es una práctica común que consiste en crear casos para probar el código y asegurar que funciona correctamente. Los programadores realizan estas pruebas para garantizar que su código funcione como se espera y evitar errores o bugs.

## Cómo:
Para escribir pruebas en Kotlin, puedes utilizar la función ```assert```, que verifica si una expresión es verdadera o falsa. Por ejemplo:

```
fun sum(x: Int, y: Int): Int {
  return x + y
}

assert(sum(2, 3) == 5) // Esta prueba debería pasar
assert(sum(1, 2) == 4) // Esta prueba debería fallar ya que la suma es incorrecta
```

Otra opción es utilizar la librería de pruebas de Kotlin, llamada ```kotlin-test```. Esta librería proporciona una gran variedad de funciones para realizar pruebas más complejas y específicas. Por ejemplo:

```
import kotlin.test.*

fun sum(x: Int, y: Int): Int {
  return x + y
}

@Test
fun testSum() {
  assertEquals(sum(2, 3), 5) // Esta prueba debería pasar
  assertNotEquals(sum(1, 2), 4) // Esta prueba debería fallar ya que la suma es incorrecta
}
```

Para ejecutar estas pruebas, puedes utilizar alguna herramienta de construcción de proyectos como Gradle o Maven, o incluso ejecutarlas directamente en la consola utilizando el comando ```kotlin -classpath path/to/kotlin-test.jar YourTest.kt```.

## Profundizando:
Escribir pruebas no es una práctica reciente, de hecho, en la década de 1970 ya se utilizaban para verificar el correcto funcionamiento de programas. Sin embargo, en la actualidad, se han popularizado gracias al desarrollo del desarrollo de metodologías ágiles y la necesidad de entregar código de alta calidad y libre de errores.

Otra alternativa para escribir pruebas en Kotlin es utilizar herramientas de testing como JUnit o Mockito, que son populares en la comunidad de desarrollo. Estas herramientas ofrecen una mayor flexibilidad y funcionalidades avanzadas para realizar pruebas más complejas.

En cuanto a la implementación de las pruebas, es importante seguir buenas prácticas como nombrar de forma clara y descriptiva las pruebas, utilizar casos de prueba variados y tener en cuenta tanto los escenarios esperados como los inesperados. Además, es recomendable escribir las pruebas antes de escribir el código, en lugar de probar una vez ya está desarrollado.

## Ver también:
- Documentación oficial de Kotlin sobre pruebas: https://kotlinlang.org/docs/reference/testing.html
- Artículo de Tania Rascia sobre cómo escribir pruebas en Kotlin: https://www.taniarascia.com/unit-testing-in-kotlin/ 
- Guía de referencia de Mockito para Kotlin: https://site.mockito.org/