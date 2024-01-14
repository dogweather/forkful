---
title:                "Kotlin: Escrevendo testes"
programming_language: "Kotlin"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Kotlin?

Quando estamos desenvolvendo um aplicativo ou sistema em Kotlin, é importante garantir que ele esteja funcionando corretamente e sem bugs. É aí que entram os testes! Escrever testes em Kotlin nos ajuda a identificar problemas em nosso código de forma mais rápida e eficiente, garantindo maior qualidade e confiabilidade em nossos projetos. Além disso, com a ajuda dos testes, podemos ter certeza de que o nosso código continuará funcionando corretamente mesmo após fazermos alterações futuras.

## Como escrever testes em Kotlin

Para escrever testes em Kotlin, podemos usar o framework de testes embutido no Kotlin, o JUnit. Vamos dar uma olhada em um exemplo simples de um teste de função usando o JUnit.

```Kotlin
fun somar(a: Int, b: Int) = a + b
```

Primeiro, vamos importar o JUnit na nossa classe de teste:

```Kotlin
import org.junit.Test
```

Em seguida, vamos criar o nosso teste da função somar:

```Kotlin
@Test
fun testSomar() {
    val resultado = somar(2, 3)
    assertEquals(5, resultado)
}
```

Aqui, usamos a anotação `@Test` para indicar que essa função é um teste. Dentro do teste, chamamos a função `somar()` passando os parâmetros 2 e 3 e em seguida, usamos a função `assertEquals()` para verificar se o resultado é igual a 5. 

Com a ajuda do JUnit, podemos criar testes para várias funções e lógicas dentro do nosso código, garantindo que tudo esteja funcionando corretamente.

## Aprofundando nos testes em Kotlin

Existem diversos tipos de testes que podemos escrever em Kotlin, como testes unitários, testes de integração e testes de aceitação. Cada um desses tipos de teste tem um propósito diferente e é importante entender em quais situações devemos utilizá-los.

Além disso, é importante também seguir boas práticas ao escrever testes em Kotlin, como manter os testes independentes e cobrir todos os possíveis cenários. Esta prática não apenas nos ajuda a encontrar bugs de forma mais rápida, mas também contribui para um código mais organizado e de fácil manutenção.

## Veja também

- [Documentação do JUnit para Kotlin](https://github.com/junit-team/junit4/wiki)
- [Tutorial de testes em Kotlin usando o JUnit](https://www.baeldung.com/kotlin/junit-5-kotlin)
- [Princípios de testes em Kotlin](https://blog.philipphauer.de/donts-writing-tests-kotlin/)

Com a ajuda destes recursos, você pode aprofundar ainda mais seus conhecimentos em testes em Kotlin e se tornar um programador ainda melhor!