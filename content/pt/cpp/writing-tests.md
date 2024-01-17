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

## O que & Porquê?

Escrever testes é simplesmente o ato de verificar se o seu código funciona como esperado. Os programadores fazem isso para garantir que o código seja confiável e está de acordo com as expectativas dos usuários.

## Como fazer:

Para escrever testes em C++, é necessário usar a biblioteca de testes padrão do C++. Aqui está um exemplo básico de um teste de unidade:

```
#include <cassert> // Inclui a biblioteca de testes do C++

int soma(int a, int b) {
  return a + b;
}

int main() {
  assert(soma(2, 2) == 4); // Verifica se a soma de 2 e 2 é igual a 4
  return 0;
}
```

A saída esperada seria "OK", indicando que o teste foi aprovado.

## Profundidade:

Escrever testes não é uma prática nova. Na verdade, ela vem sendo utilizada há décadas no desenvolvimento de software. Existem outras bibliotecas de teste disponíveis além da biblioteca padrão do C++, como o Google Test e o Catch2, que oferecem recursos adicionais.

Além disso, é possível implementar testes automatizados, que podem ser executados sempre que o código for alterado, garantindo que os testes sejam executados frequentemente e de forma consistente.

## Veja também:

- [Documentação da biblioteca de testes do C++](https://en.cppreference.com/w/cpp/header/cassert)
- [Google Test](https://github.com/google/googletest)
- [Catch2](https://github.com/catchorg/Catch2)