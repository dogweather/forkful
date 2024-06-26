---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:08.635411-07:00
description: "Como Fazer: O Kotlin suporta o desenvolvimento guiado por testes com\
  \ v\xE1rios frameworks, sendo os mais populares o JUnit, o Kotest e o MockK para\
  \ mock\u2026"
lastmod: '2024-03-13T22:44:46.545496-06:00'
model: gpt-4-0125-preview
summary: "O Kotlin suporta o desenvolvimento guiado por testes com v\xE1rios frameworks,\
  \ sendo os mais populares o JUnit, o Kotest e o MockK para mock (simula\xE7\xE3\
  o)."
title: Escrevendo testes
weight: 36
---

## Como Fazer:
O Kotlin suporta o desenvolvimento guiado por testes com vários frameworks, sendo os mais populares o JUnit, o Kotest e o MockK para mock (simulação). Aqui está um exemplo simples usando JUnit:

```kotlin
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class CalculatorTest {

    @Test
    fun `soma dois números`() {
        val calculator = Calculator()
        val result = calculator.add(2, 3)
        assertEquals(5, result)
    }
}

class Calculator {
    fun add(a: Int, b: Int): Int = a + b
}
```

**Saída de Exemplo**

```text
Teste passou.
```

Para uma abordagem de teste mais sofisticada usando o Kotest, que oferece um estilo de escrita de teste mais idiomático para Kotlin, veja o exemplo abaixo:

```kotlin
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe

class CalculatorSpec : StringSpec({
    "somar 2 e 3 deve retornar 5" {
        val calculator = Calculator()
        calculator.add(2, 3) shouldBe 5
    }
})
```

Usando o MockK para testar com mocks:

```kotlin
import io.mockk.every
import io.mockk.mockk
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class ServiceTest {

    private val repository = mockk<Repository>()
    private val service = Service(repository)

    @Test
    fun `obter dados retorna dados simulados`() {
        every { repository.getData() } returns "Dados Simulados"

        val result = service.getData()

        assertEquals("Dados Simulados", result)
    }
}

class Service(private val repository: Repository) {
    fun getData(): String = repository.getData()
}

interface Repository {
    fun getData(): String
}
```

**Saída de Exemplo**

```text
Teste passou.
```

Esses exemplos ilustram os conceitos básicos de escrita de testes unitários em Kotlin. À medida que sua aplicação cresce, considere explorar técnicas e ferramentas de teste mais avançadas fornecidas por cada framework.
