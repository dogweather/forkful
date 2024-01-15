---
title:                "Gerando números aleatórios."
html_title:           "C++: Gerando números aleatórios."
simple_title:         "Gerando números aleatórios."
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que

Gerar números aleatórios é útil em muitos programas diferentes. Pode ser usado para criar senhas de forma segura, fazer simulações e testes, e até mesmo para jogos de azar.

## Como fazer

A geração de números aleatórios pode ser feita utilizando a biblioteca padrão de C++, `cstdlib`. Primeiro, será necessário incluir a biblioteca no início do código:

```C++

#include <cstdlib>

```

Em seguida, podemos utilizar a função `rand()` para gerar um número aleatório:

```C++
// Gerando um número aleatório entre 0 e 10 (não incluindo o 10)
int numero = rand() % 10;

// Gerando um número aleatório entre 1 e 100 (incluindo o 100)
numero = rand() % 100 + 1;
```

Para garantir que os números gerados sejam realmente aleatórios, é importante inicializar a semente da função `rand()` usando a função `srand()` e o tempo atual:

```C++
// Inicializando a semente com o tempo atual
srand(time(NULL));

// Gerando um número aleatório entre 1 e 50 (incluindo o 50)
numero = rand() % 50 + 1;
```

Podem ser feitas várias manipulações com o uso de operações matemáticas, como adição ou multiplicação, para gerar diferentes limites e faixas de números aleatórios.

## Mergulho profundo

É importante entender que a função `rand()` não gera números verdadeiramente aleatórios. Ela utiliza um algoritmo baseado em uma semente inicial para gerar uma sequência pseudoaleatória. Isso significa que, se a semente inicial for a mesma, a sequência de números gerados também será a mesma.

Por esse motivo, é importante sempre inicializar a semente de forma diferente a cada execução do programa, para garantir que os números gerados sejam o mais aleatórios possíveis.

## Veja também

- [Documentação da função `rand()` (em inglês)](http://www.cplusplus.com/reference/cstdlib/rand/)
- [Outras formas de gerar números aleatórios em C++ (em inglês)](https://www.learncpp.com/cpp-tutorial/59-random-number-generation/)
- [Explicações mais detalhadas sobre pseudoaleatoriedade (em inglês)](https://www.azillionmonkeys.com/qed/random.html)