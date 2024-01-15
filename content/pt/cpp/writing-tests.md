---
title:                "Escrevendo testes"
html_title:           "C++: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em C++

Testes são uma parte importante do processo de desenvolvimento de software, principalmente em linguagens como C++ onde erros podem ser mais difíceis de serem identificados. Além disso, escrever testes pode ajudar a garantir a qualidade do código e reduzir o tempo gasto em depuração.

## Como escrever testes em C++

Para escrever testes efetivos em C++, é necessário usar uma estrutura de teste, como o [Catch](https://github.com/catchorg/Catch2). Veja um exemplo de como seria um teste simples em C++ usando Catch:

```C++
#define CATCH_CONFIG_MAIN
#include "catch.hpp"

// Função que será testada
int square(int x) {
  return x * x;
}

// Casos de teste
TEST_CASE("Square function", "[square]") {
  REQUIRE(square(2) == 4); //Verifica se o resultado é o esperado
  REQUIRE(square(-3) == 9);
  REQUIRE(square(0) == 0);
}
```
Ao executar esse teste, o output seria:

```
===============================================================================
All tests passed (3 assertions in 1 test case)

```

## Mergulho Profundo

Ao escrever testes em C++, é importante seguir algumas práticas para garantir que eles sejam efetivos. Primeiramente, os casos de teste devem cobrir todos os possíveis cenários de uso da função ou código testado. Além disso, é importante verificar se o resultado esperado é igual ao resultado obtido, usando a macro `REQUIRE()` como no exemplo acima. Também é recomendável utilizar mensagens claras e significativas para os casos de teste, facilitando a identificação de erros no código.

## Veja também

- [Catch Framework](https://github.com/catchorg/Catch2)
- [Como escrever testes efetivos em C++](https://www.infoworld.com/article/3562151/6-steps-to-writing-world-class-code-test-driven-development.html)