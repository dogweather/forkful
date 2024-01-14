---
title:    "C++: Escrevendo testes"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em C++?

Ao trabalhar com linguagens de programação, é importante garantir que seu código esteja funcionando corretamente e livre de erros. Escrever testes em C++ é uma forma valiosa de garantir a qualidade do seu código e identificar possíveis problemas antes deles chegarem aos usuários finais. Além disso, testes bem escritos podem ajudar na detecção de bugs em atualizações futuras e economizar tempo e esforço no processo de depuração.

## Como escrever testes em C++?

Existem algumas bibliotecas populares disponíveis para escrever testes em C++, como o Google Test e o Catch2. Essas bibliotecas fornecem ferramentas e funções para criar e executar testes de forma fácil e eficiente. Vamos dar uma olhada em um exemplo de teste usando o Google Test:

```C++
#include <gtest/gtest.h>

TEST(MathTest, Addition) {
    // inputs
    int x = 5;
    int y = 8;

    // expected result
    int expected = 13;

    // execute function
    int actual = x + y;

    // assert result
    ASSERT_EQ(expected, actual);
}

// Output: [ RUN ] MathTest.Addition
// Output: [       OK ] MathTest.Addition (0 ms total)
```

Neste exemplo, estamos testando se a soma de dois números resulta no valor esperado. Podemos adicionar mais testes no mesmo arquivo ou em arquivos separados para diferentes funcionalidades.

## Mergulho profundo

Para escrever testes eficazes, é importante considerar diferentes cenários e testar todas as possibilidades do seu código. É também uma boa prática manter os testes atualizados com o código, para garantir que eles sejam executados corretamente em atualizações futuras.

Outro aspecto importante é usar nomes descritivos para seus testes, para facilitar a identificação de possíveis problemas. Além disso, é recomendado testar funções ou métodos individuais em vez de testar a funcionalidade completa, pois isso torna mais fácil identificar a localização do erro.

## Veja também

- [Documentação do Google Test](https://github.com/google/googletest)
- [Tutorial do Catch2](https://github.com/catchorg/Catch2/blob/master/docs/tutorial.md)
- [Por que escrever testes: benefícios e melhores práticas](https://www.tricentis.com/blog/why-write-unit-tests-benefits-best-practices/)