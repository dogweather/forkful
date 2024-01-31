---
title:                "Geração de números aleatórios"
date:                  2024-01-27T20:32:54.404277-07:00
model:                 gpt-4-0125-preview
simple_title:         "Geração de números aleatórios"

category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Quê e Por Quê?

Gerar números aleatórios em C envolve criar sequências de números que não possuem nenhum padrão discernível, imitando o conceito de aleatoriedade. Os programadores usam números aleatórios para uma infinidade de finalidades, incluindo simulação de dados, aplicações criptográficas e desenvolvimento de jogos, tornando isso um aspecto vital da programação.

## Como Fazer:

Para gerar números aleatórios em C, você normalmente usa a função `rand()` encontrada em `stdlib.h`. No entanto, é crucial semear o gerador de números aleatórios para assegurar a variabilidade nos números gerados entre diferentes execuções do programa. A função `srand()`, semeada com um valor, muitas vezes o tempo atual, facilita isso.

Aqui está um exemplo simples de como gerar um número aleatório entre 0 e 99:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Semeia o gerador de números aleatórios
    srand((unsigned) time(NULL));

    // Gera um número aleatório entre 0 e 99
    int randomNumber = rand() % 100;

    // Imprime o número aleatório
    printf("Número Aleatório: %d\n", randomNumber);

    return 0;
}
```

Saída de exemplo:

```
Número Aleatório: 42
```

É importante notar que cada execução deste programa produzirá um novo número aleatório, graças à semeadura com o tempo atual.

## Aprofundamento

A maneira tradicional de gerar números aleatórios em C, usando `rand()` e `srand()`, não é verdadeiramente aleatória. É pseudorrandômica. Isso é suficiente para muitas aplicações, mas fica aquém em situações que exigem altos graus de aleatoriedade, como em usos criptográficos sérios. A sequência gerada por `rand()` é totalmente determinada pela semente fornecida a `srand()`. Assim, se a semente for conhecida, a sequência pode ser prevista, reduzindo a aleatoriedade.

Historicamente, a função `rand()` foi criticada por sua baixa qualidade de aleatoriedade e alcance limitado. Alternativas modernas incluem usar APIs específicas do dispositivo ou bibliotecas externas que aproximam melhor a verdadeira aleatoriedade ou, em sistemas semelhantes ao UNIX, ler de `/dev/random` ou `/dev/urandom` para fins criptográficos.

Por exemplo, usando `/dev/urandom` em C:

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    unsigned int randomNumber;

    // Abre /dev/urandom para leitura
    fp = fopen("/dev/urandom", "r");

    // Lê um número aleatório
    fread(&randomNumber, sizeof(randomNumber), 1, fp);

    // Imprime o número aleatório
    printf("Número Aleatório: %u\n", randomNumber);

    // Fecha o arquivo
    fclose(fp);

    return 0;
}
```

Este método lê diretamente do pool de entropia do sistema, oferecendo uma qualidade de aleatoriedade mais elevada, adequada para aplicações mais sensíveis. No entanto, essa abordagem pode ter problemas de portabilidade em diferentes plataformas, tornando-a menos universal que o uso de `rand()`.

Independente do método, entender a natureza da aleatoriedade e sua implementação em C é crucial para desenvolver aplicações eficazes, seguras e envolventes.
