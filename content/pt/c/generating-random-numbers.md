---
title:                "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios em C?

Gerar números aleatórios é uma técnica fundamental em programação, especialmente em jogos e simulações. Com a capacidade de gerar números aleatórios em C, podemos adicionar um elemento de imprevisibilidade aos nossos programas, tornando-os mais dinâmicos e interessantes para o usuário.

## Como gerar números aleatórios em C?

A geração de números aleatórios em C é feita por meio da função `rand()`, que retorna um inteiro aleatório entre 0 e `RAND_MAX`. No entanto, para obtermos um número entre um intervalo específico, devemos usar a fórmula `rand() % (max + 1 - min) + min`.

Veja um exemplo abaixo:

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
    // Gerando um número aleatório entre 1 e 10
    int randomNumber = rand() % 10 + 1;
    printf("Número aleatório gerado: %d\n", randomNumber);
    return 0;
}

```

O output do código acima pode ser algo como: `Número aleatório gerado: 7`.

## Mergulho profundo em geração de números aleatórios

A função `rand()` usa uma técnica conhecida como *pseudo-random number generation*, que gera uma sequência de números aparentemente aleatórios a partir de uma semente inicial. Essa semente pode ser definida pelo usuário por meio da função `srand()`, usando um número inteiro como parâmetro.

Outro detalhe importante é que a função `rand()` não gera números verdadeiramente aleatórios, mas sim uma sequência que pode ser reproduzida. Portanto, se quisermos obter números diferentes a cada execução do programa, devemos fornecer uma semente diferente para a função `srand()`.

## Veja também

- [Documentação oficial sobre a função `rand()` em C](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Tutorial sobre geração de números aleatórios em C](https://www.guru99.com/c-random-number-generation.html)
- [Vídeo explicando o conceito de números pseudo-aleatórios em computação](https://www.youtube.com/watch?v=5E1dRHn8ugg)