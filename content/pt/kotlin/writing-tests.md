---
title:                "Escrevendo testes"
html_title:           "Arduino: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Escrever testes é criar verificações automatizadas para o seu código, garantindo que ele funcione como esperado. Programadores fazem isso para aumentar a confiança no software, facilitar a manutenção e permitir atualizações sem medo de quebrar funcionalidades existentes.

## Como Fazer:
Para começar com testes em Kotlin, você usará o JUnit. Primeiro, adicione a dependência no seu `build.gradle`:

```kotlin
dependencies {
    testImplementation("org.junit.jupiter:junit-jupiter:5.7.0")
}
```

Agora, escreva um teste simples para avaliar uma função que soma dois números:

```kotlin
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class CalculadoraTest {

    @Test
    fun `soma de dois numeros`() {
        val esperado = 4
        val resultado = soma(2, 2)
        assertEquals(esperado, resultado)
    }

    fun soma(a: Int, b: Int) = a + b
}
```

Execute os testes e veja o resultado:

```
Test passed
```

## Mergulho Profundo:
O JUnit é o framework mais popular para testes em Java e Kotlin, originário nos anos 90. Alternativas incluem TestNG e Spek. Importante entender que bons testes devem ser fáceis de escrever e ler, rápidos para executar e confiáveis. Eles devem cobrir casos comuns e os cantos escondidos do código.

## Veja Também:
Para aprender mais sobre testes unitários em Kotlin, confira:

- Documentação oficial do JUnit: https://junit.org/junit5/
- Kotlin testing com MockK: https://mockk.io/
- Testes de integração com Testcontainers: https://www.testcontainers.org/
- Kotlin Academy sobre testes: https://blog.kotlin-academy.com/
