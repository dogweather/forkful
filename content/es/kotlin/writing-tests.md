---
title:                "Escribiendo pruebas"
date:                  2024-01-19
html_title:           "Arduino: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"

category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Qué y por qué?
Las pruebas en programación sirven para verificar que el código funcione correctamente. Los programadores las escriben para evitar errores, asegurar la calidad del código y facilitar mantenimientos futuros.

## Cómo hacerlo:
Para introducir pruebas en Kotlin, usaremos JUnit, un framework popular para pruebas unitarias.

Primero, agrega la dependencia de JUnit en tu archivo `build.gradle.kts`:

```kotlin
dependencies {
    testImplementation("org.junit.jupiter:junit-jupiter:5.8.1")
}
```

Aquí un ejemplo de una prueba simple en Kotlin:

```kotlin
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class CalculadoraTest {
    
    @Test
    fun `sumar dos numeros retorna el resultado correcto`() {
        val resultado = sumar(2, 3)
        assertEquals(5, resultado, "2 + 3 debe ser igual a 5")
    }

    fun sumar(a: Int, b: Int): Int {
        return a + b
    }
}
```

Ejecuta tus pruebas con el siguiente comando:

```shell
./gradlew test
```

Si la prueba pasa satisfactoriamente, el resultado será algo como esto:

```
> Task :test

CalculadoraTest > sumar dos numeros retorna el resultado correcto() PASSED
```

## Profundización
Las pruebas unitarias se popularizaron con frameworks como JUnit en Java, y en Kotlin seguimos este legado. Existen alternativas en Kotlin como Kotest y Spek que ofrecen una sintaxis más idiomática de Kotlin. Detalles importantes al escribir pruebas incluyen la confiabilidad, es decir, que las pruebas siempre deben dar el mismo resultado si el código no cambia, y la cobertura de código, que mide qué tan bien las pruebas exploran el código fuente.

## Ver También
- Documentación de JUnit 5: [https://junit.org/junit5/docs/current/user-guide/](https://junit.org/junit5/docs/current/user-guide/)
- Kotest, una poderosa librería de pruebas para Kotlin: [https://kotest.io/](https://kotest.io/)
- Spek, un framework de especificaciones para Kotlin: [https://www.spekframework.org/](https://www.spekframework.org/)
- Guía sobre cobertura de código con Jacoco en Kotlin: [https://www.baeldung.com/jacoco](https://www.baeldung.com/jacoco)
