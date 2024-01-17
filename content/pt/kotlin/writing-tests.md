---
title:                "Escrevendo testes"
html_title:           "Kotlin: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## O que e por que?

Escrever testes é uma prática comum entre os programadores para garantir que seu código funciona corretamente. Testes são pedaços de código que verificam se as funcionalidades criadas pelos desenvolvedores estão produzindo os resultados esperados. Isso pode evitar erros indesejados e garantir a qualidade do código.

## Como fazer:

Para escrever testes em Kotlin, podemos usar a biblioteca de testes integrada no framework de desenvolvimento Kotlin, o JUnit. Vamos assumir que temos uma classe chamada "Calculadora" com um método "somar" que recebe dois números inteiros e retorna sua soma. O teste em Kotlin seria escrito da seguinte forma:

```Kotlin
class CalculadoraTest {
    
    @Test
    fun testSomar() {
        val calculadora = Calculadora()
        assertEquals(4, calculadora.somar(2,2))
    }
}
```

O método "assertEquals" do JUnit compara o resultado da operação com o valor esperado. Se forem iguais, o teste passa. Se forem diferentes, o teste falha. É possível escrever vários testes para diferentes cenários, como por exemplo, testar se a calculadora funciona corretamente com números negativos ou decimais.

## Profundando:

A prática de escrever testes foi introduzida no desenvolvimento de software na década de 1980. Antes disso, os programas eram testados manualmente por suas equipes de desenvolvimento, o que levava a muitos erros e atrasos no lançamento de produtos. Atualmente, existem outras alternativas para escrever testes em Kotlin, como o framework de teste Spek ou a ferramenta MockK para testar classes com dependências.

Para implementar testes em Kotlin, é importante criar classes de teste separadas da classe principal e utilizar as anotações "@Test" para indicar quais métodos devem ser executados como testes. Também é possível adicionar anotações como "@Before" ou "@After" para executar certas ações antes ou depois dos testes, como inicializar variáveis ou limpar dados. Além disso, podemos utilizar o recurso de "asserções personalizadas" do JUnit para criar nossas próprias verificações de resultados.

## Veja também:

- [Documentação oficial do JUnit para Kotlin](https://junit.org/junit5/docs/current/user-guide/#overview) 
- [Framework de teste Spek para Kotlin](https://spekframework.org/) 
- [Ferramenta MockK para testar classes com dependências em Kotlin](https://mockk.io/)