---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:08.635411-07:00
description: "Escrever testes em Kotlin envolve a cria\xE7\xE3o de trechos de c\xF3\
  digo que validam automaticamente a corre\xE7\xE3o funcional dos seus m\xF3dulos\
  \ de software, garantindo\u2026"
lastmod: 2024-02-19 22:05:05.585407
model: gpt-4-0125-preview
summary: "Escrever testes em Kotlin envolve a cria\xE7\xE3o de trechos de c\xF3digo\
  \ que validam automaticamente a corre\xE7\xE3o funcional dos seus m\xF3dulos de\
  \ software, garantindo\u2026"
title: Escrevendo testes
---

{{< edit_this_page >}}

## O Que & Por Quê?

Escrever testes em Kotlin envolve a criação de trechos de código que validam automaticamente a correção funcional dos seus módulos de software, garantindo que eles funcionem conforme esperado. Os programadores fazem isso para detectar bugs precocemente, facilitar a refatoração do código e fornecer documentação sobre como os componentes de software devem funcionar.

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
