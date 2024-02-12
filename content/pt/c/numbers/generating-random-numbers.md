---
title:                "Gerando números aleatórios"
aliases: - /pt/c/generating-random-numbers.md
date:                  2024-02-03T17:57:13.823779-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gerando números aleatórios"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/generating-random-numbers.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Porquê?

Gerar números aleatórios em C envolve a criação de valores que são imprevisíveis e seguem uma distribuição específica, como uniforme ou normal. Essa capacidade é crucial para aplicações que vão desde simulações e jogos até operações criptográficas, onde a imprevisibilidade ou a simulação de aleatoriedade do mundo real é essencial.

## Como fazer:

Em C, números aleatórios podem ser gerados usando a função `rand()`, que faz parte da biblioteca padrão do C `<stdlib.h>`. Por padrão, `rand()` produz números pseudoaleatórios no intervalo de 0 a `RAND_MAX` (uma constante definida em `<stdlib.h>`). Para ter mais controle sobre o intervalo, os programadores podem manipular a saída de `rand()`.

Aqui está um exemplo simples de geração de um número aleatório entre 0 e 99:

```c
#include <stdio.h>
#include <stdlib.h> // Para rand() e srand()
#include <time.h>   // Para time()

int main() {
    // Sementeia o gerador de números aleatórios
    srand((unsigned) time(NULL));

    // Gera um número aleatório entre 0 e 99
    int randomNumber = rand() % 100;

    printf("Número Aleatório: %d\n", randomNumber);

    return 0;
}
```

A saída do exemplo pode variar a cada vez que você executa este programa:

```
Número Aleatório: 42
```
Para gerar números aleatórios em um intervalo diferente, você pode ajustar o operador de módulo (`%`) de acordo. Por exemplo, `rand() % 10` gera números de 0 a 9.

É importante notar que semear o gerador de números pseudoaleatórios (`srand()`) com o tempo atual (`time(NULL)`) garante sequências diferentes de números aleatórios a cada execução do programa. Sem a semeadura (`srand()`), `rand()` produziria a mesma sequência de números todas as vezes que o programa fosse executado.

## Aprofundamento

A função `rand()` e seu par para semeadura `srand()` fazem parte da biblioteca padrão do C há décadas. Eles são baseados em algoritmos que geram sequências de números que apenas parecem ser aleatórios—daí o termo "pseudoaleatórios". O algoritmo subjacente em `rand()` é tipicamente um gerador linear congruencial (LCG).

Embora `rand()` e `srand()` sejam suficientes para muitas aplicações, eles têm limitações conhecidas, especialmente no que diz respeito à qualidade da aleatoriedade e à potencial previsibilidade. Para aplicações que exigem aleatoriedade de alta qualidade, como operações criptográficas, alternativas como `/dev/random` ou `/dev/urandom` (em sistemas semelhantes ao Unix), ou APIs fornecidas por bibliotecas criptográficas, devem ser consideradas.

Com a introdução do C11, o padrão ISO C incluiu um novo cabeçalho, `<stdatomic.h>`, oferecendo um controle mais refinado para operações concorrentes, mas não diretamente relacionado à aleatoriedade. Para verdadeira aleatoriedade em C, desenvolvedores muitas vezes recorrem a bibliotecas específicas da plataforma ou externas que oferecem algoritmos melhores ou fazem uso de fontes de entropia de hardware.

Lembre-se, enquanto `rand()` serve como um meio simples e acessível para gerar números pseudoaleatórios, seus usos em aplicações modernas são limitados pela qualidade e previsibilidade de sua saída. Quando soluções mais robustas são necessárias, especialmente para aplicações conscientes da segurança, explorar além da biblioteca padrão é altamente recomendado.
