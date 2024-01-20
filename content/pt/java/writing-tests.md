---
title:                "Escrevendo testes"
html_title:           "Java: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/writing-tests.md"
---

{{< edit_this_page >}}

## O que é e por que fazer?

Escrever testes é uma prática importante para os programadores, pois permite garantir que o código está funcionando corretamente e evitar possíveis bugs. É um processo de criação de pequenos programas que verificam se o código principal está produzindo os resultados esperados.

## Como fazer:

Para criar testes em Java, é necessário criar uma classe de teste que herda de JUnit, uma biblioteca de testes para Java. Dentro dessa classe, é possível utilizar os métodos de asserção para verificar se os resultados obtidos são iguais aos esperados. Por exemplo:

```Java
@Test
public void testAdicao() {
    int resultado = Calculadora.adicionar(2, 2);
    assertEquals(4, resultado);
}
```
Esse teste verifica se a função "adicionar" da classe "Calculadora" está retornando o resultado correto.

## Mais detalhes:

Os testes automatizados surgiram com a chamada "Programação Extrema" nos anos 90, como uma forma de garantir a qualidade do código em um cenário de desenvolvimento ágil. Existem outras ferramentas para testes em Java, como TestNG e Mockito, porém JUnit ainda é o mais utilizado.

Além disso, existem diferentes abordagens para escrever testes, como a Test Driven Development (TDD), em que os testes são escritos antes do código, e a Behavior Driven Development (BDD), que foca nos comportamentos esperados do software.

## Veja também:

- [JUnit Documentation](https://junit.org/junit5/docs/current/user-guide/)
- [Mockito](https://site.mockito.org/)