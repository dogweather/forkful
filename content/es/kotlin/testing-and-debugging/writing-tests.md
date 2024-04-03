---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:15.971426-07:00
description: "Escribir pruebas en Kotlin implica elaborar fragmentos de c\xF3digo\
  \ que validan autom\xE1ticamente la correcci\xF3n funcional de tus m\xF3dulos de\
  \ software,\u2026"
lastmod: '2024-03-13T22:44:59.039428-06:00'
model: gpt-4-0125-preview
summary: "Escribir pruebas en Kotlin implica elaborar fragmentos de c\xF3digo que\
  \ validan autom\xE1ticamente la correcci\xF3n funcional de tus m\xF3dulos de software,\
  \ asegurando que funcionen como se espera."
title: Escribiendo pruebas
weight: 36
---

## Cómo hacerlo:
Kotlin soporta el desarrollo guiado por pruebas con varios marcos de trabajo, siendo los más populares JUnit, Kotest y MockK para el mockeo. Aquí hay un ejemplo simple utilizando JUnit:

```kotlin
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class CalculatorTest {

    @Test
    fun `suma dos números`() {
        val calculator = Calculator()
        val result = calculator.add(2, 3)
        assertEquals(5, result)
    }
}

class Calculator {
    fun add(a: Int, b: Int): Int = a + b
}
```

**Salida de muestra**

```text
Prueba pasada.
```

Para un enfoque de prueba más sofisticado utilizando Kotest, que ofrece un estilo de escritura de prueba más idiomático de Kotlin, vea el ejemplo a continuación:

```kotlin
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe

class CalculatorSpec : StringSpec({
    "sumar 2 y 3 debería retornar 5" {
        val calculator = Calculator()
        calculator.add(2, 3) shouldBe 5
    }
})
```

Usando MockK para pruebas con mocks:

```kotlin
import io.mockk.every
import io.mockk.mockk
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class ServiceTest {

    private val repository = mockk<Repository>()
    private val service = Service(repository)

    @Test
    fun `obtener datos retorna datos simulados`() {
        every { repository.getData() } returns "Datos Simulados"

        val result = service.getData()

        assertEquals("Datos Simulados", result)
    }
}

class Service(private val repository: Repository) {
    fun getData(): String = repository.getData()
}

interface Repository {
    fun getData(): String
}
```

**Salida de muestra**

```text
Prueba pasada.
```

Estos ejemplos ilustran los conceptos básicos de escribir pruebas unitarias en Kotlin. A medida que tu aplicación crezca, considera explorar técnicas y herramientas de prueba más avanzadas proporcionadas por cada marco de trabajo.
