---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:48:49.021458-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O que é & Porquê?
Gerar números aleatórios é o processo de criar valores imprevisíveis via computador. Programadores fazem isso por diversas razões, desde adicionar aleatoriedade em jogos até realizar simulações e criptografia.

## Como fazer:
Aqui está um exemplo simples em C para gerar um número aleatório:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Inicializa o gerador de números aleatórios
    srand(time(NULL));

    // Gera um número aleatório entre 0 e 99
    int numeroAleatorio = rand() % 100;

    // Imprime o número aleatório
    printf("Número aleatório: %d\n", numeroAleatorio);

    return 0;
}
```

A saída será um número entre `0` e `99`, que muda a cada execução.

## Aprofundamento:
A função `rand()` é usada desde os primórdios da linguagem C para gerar números aleatórios, mas não é ideal para situações que exigem alta segurança ou que a sequência seja totalmente imprevisível. Alternativas modernas incluem o uso de `/dev/random` ou `/dev/urandom` em sistemas Unix-like ou APIs de criptografia para números aleatórios mais seguros.

Outro ponto é que `rand()` por si só sempre gerará a mesma sequência de números após cada inicialização do programa. Para evitar isso, usamos `srand()` com a hora atual como semente (seed), tornando a sequência difícil de prever. A qualidade da aleatoriedade gerada pelo `rand()` depende da implementação da biblioteca C e pode variar entre diferentes sistemas operacionais ou versões de compiladores.

## Ver também:
- Documentação da GNU sobre `rand()` e `srand()`: https://www.gnu.org/software/libc/manual/html_node/ISO-Random.html
- Um artigo discutindo aleatoriedade e funções de C: https://www.ime.usp.br/~pf/algoritmos/aulas/random.html
- Perguntas frequentes sobre números aleatórios em C: https://stackoverflow.com/questions/tagged/random+in+c
