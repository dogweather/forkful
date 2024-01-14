---
title:                "Kotlin: Escrevendo Testes"
simple_title:         "Escrevendo Testes"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante

Escrever testes é uma prática essencial na programação, pois garante que o seu código funciona corretamente e que alterações futuras não irão quebrá-lo. Além disso, testes bem escritos ajudam a identificar e corrigir erros de forma mais rápida e eficiente. 

## Como escrever testes em Kotlin

Para escrever testes em Kotlin, é necessário utilizar a biblioteca nativa de testes, JUnit. Primeiramente, declare uma função de teste utilizando a anotação `@Test` e, dentro dela, utilize a função `assertEquals()` para avaliar se o resultado obtido é igual ao esperado. Veja um exemplo abaixo:

```Kotlin
@Test
fun testSum() {
    val result = sum(2, 3)
    assertEquals(5, result)
}
```

Neste caso, a função `sum()` está sendo testada e o resultado esperado é que a soma de 2 e 3 seja igual a 5. Além disso, é possível utilizar outras funções de comparação, como `assertTrue()` e `assertNotNull()`, de acordo com a necessidade do teste.

## Aprofundando-se na escrita de testes

Escrever testes é uma tarefa importante, mas pode se tornar ainda mais valiosa ao utilizar algumas boas práticas. É recomendado escrever testes antes mesmo de começar a implementar o código, pois isso ajuda a definir e compreender melhor os requisitos e comportamentos esperados. Além disso, é importante garantir a cobertura dos testes, ou seja, verificar se todos os possíveis cenários do código estão sendo testados. 

Outro ponto importante é a manutenção dos testes. Quando ocorrem alterações no código, é essencial atualizar os testes para garantir que eles ainda estejam avaliando o comportamento correto. Por fim, é válido mencionar a utilização de ferramentas de automação de testes, que podem ajudar a reduzir o tempo e o esforço investidos na escrita de testes.

## Veja também

- [JUnit User Guide](https://junit.org/junit5/docs/current/user-guide/)
- [Writing Good Tests - Best Practices](https://codeburst.io/writing-good-tests-best-practices-using-bdd-162a37051da0)
- [How to Write Better Tests](https://medium.com/javascript-scene/how-to-write-better-tests-fa683a5f44d5)