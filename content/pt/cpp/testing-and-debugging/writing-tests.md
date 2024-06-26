---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:38.931659-07:00
description: "Como Fazer: Uma das bibliotecas de terceiros mais populares para escrever\
  \ testes em C++ \xE9 o Google Test. Primeiramente, voc\xEA precisar\xE1 instalar\
  \ o Google\u2026"
lastmod: '2024-03-13T22:44:46.883077-06:00'
model: gpt-4-0125-preview
summary: "Uma das bibliotecas de terceiros mais populares para escrever testes em\
  \ C++ \xE9 o Google Test."
title: Escrevendo testes
weight: 36
---

## Como Fazer:


### Usando o Framework de Testes do Google
Uma das bibliotecas de terceiros mais populares para escrever testes em C++ é o Google Test. Primeiramente, você precisará instalar o Google Test e vinculá-lo ao seu projeto. Uma vez configurado, você pode começar a escrever casos de teste.

```cpp
#include <gtest/gtest.h>

int add(int a, int b) {
    return a + b;
}

TEST(TestSuiteName, TestName) {
    EXPECT_EQ(3, add(1, 2));
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

Salve o código em um arquivo e compile-o com o compilador g++, vinculando a biblioteca do Google Test. Se tudo estiver configurado corretamente, a execução do executável resultante executará o teste, e se a função `add` funcionar conforme esperado, você verá algo como:

```
[==========] Executando 1 teste de 1 suite de testes.
[----------] Configuração global do ambiente de teste.
[----------] 1 teste de TestSuiteName
[ EXECUTANDO] TestSuiteName.TestName
[   OK   ] TestSuiteName.TestName (0 ms)
[----------] 1 teste de TestSuiteName (0 ms no total)

[==========] 1 teste de 1 suite de testes foi executado. (1 ms no total)
[  PASSOU  ] 1 teste.
```

### Usando Catch2
Outro framework de testes popular para C++ é o Catch2. Ele possui uma sintaxe mais simples e geralmente não requer vinculação com uma biblioteca (apenas cabeçalhos). Aqui está um exemplo de como escrever um teste simples com Catch2:

```cpp
#define CATCH_CONFIG_MAIN  // Isso indica ao Catch para fornecer um main() - faça isso apenas em um arquivo cpp
#include <catch.hpp>

int multiply(int a, int b) {
    return a * b;
}

TEST_CASE( "Inteiros são multiplicados", "[multiply]" ) {
    REQUIRE( multiply(2, 3) == 6 );
}
```

Ao compilar e executar este teste, o Catch2 fornece uma saída clara indicando se o teste passou ou falhou, junto com as informações necessárias para depurar falhas:

```
===============================================================================
Todos os testes passaram (1 assertiva em 1 caso de teste)
```

Estes exemplos mostram como a integração de frameworks de teste no seu fluxo de trabalho de desenvolvimento C++ pode melhorar significativamente a confiabilidade e a manutenção do seu código.
