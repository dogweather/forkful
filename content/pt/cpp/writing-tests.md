---
title:                "C++: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante para programadores?

Escrever testes é uma parte essencial da programação. Ao testar nosso código, podemos identificar e corrigir bugs e problemas antes que nosso programa seja lançado ou usado por outros. Além disso, escrever testes ajuda a garantir que nosso código seja capaz de lidar com situações inesperadas e melhora a qualidade geral do nosso trabalho.

## Como escrever testes em C++

Escrever testes em C++ pode parecer uma tarefa assustadora, mas com a estrutura certa, pode ser muito fácil. Aqui está um exemplo simples de um teste usando a biblioteca Catch2:

```C++

#define CATCH_CONFIG_MAIN
#include <catch2/catch.hpp>

// Função para adicionar dois números
int addNumbers(int a, int b) {
    return a + b;
}

TEST_CASE( "Somando dois números", "[adicionar]" ) {
    REQUIRE( addNumbers(2, 3) == 5 );
}

```

O código acima usa a biblioteca Catch2 para definir um teste com o nome "Somando dois números" que verifica se a função `addNumbers` retorna o resultado correto. Ao executar este teste, se o resultado for diferente de 5, o teste falhará e nos informará qual resultado foi retornado. É assim que os testes nos ajudam a encontrar e corrigir bugs em nosso código.

## Aprofundando nos testes

Escrever testes não é apenas sobre verificação de resultados. Também é importante testar as diferentes possibilidades e cobrir o máximo de código possível. Por exemplo, podemos adicionar testes para diferentes tipos de entrada, como números negativos ou zero, e também testar situações de erro, como valores inválidos sendo passados para uma função. Quanto mais testarmos nosso código, maior a chance de encontrar e corrigir problemas antes que eles se tornem um grande obstáculo.

## Veja também

- [Documentação da biblioteca Catch2](https://github.com/catchorg/Catch2/blob/master/docs/Readme.md)
- [Tutorial de teste de unidade em C++](https://devblogs.microsoft.com/cppblog/unit-testing-in-visual-studio-made-easy/)
- [Tutorial de teste de unidade usando a biblioteca Boost.Test](https://www.boost.org/doc/libs/1_76_0/libs/test/doc/html/index.html)