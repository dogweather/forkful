---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Geração de Números Aleatórios em C
---

## O Que & Porquê?

Geração de números aleatórios é o processo de criação de números de forma imprevisível. Programadores fazem isso para simular eventos naturais, adicionar imprevisibilidade ao software, ou para fins de segurança.

## Como Fazer:

Aqui estão algumas maneiras comuns de gerar números aleatórios em C:

1. Usando a função rand():

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    srand(time(0));   // Inicializa gerador de números aleatórios
    int num = rand(); // Gera um número aleatório

    printf("%d", num);
    
    return 0;
}
```

A saída será um número aleatório a cada execução do programa.

2. Geração de um número aleatório dentro de um intervalo especificado:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    srand(time(0));   
    int num = rand() % 100; // Gera um número aleatório entre 0 e 99

    printf("%d", num); 

    return 0;
}
```
Aqui a saída será um número aleatório entre 0 e 99.

## Mergulho Profundo:

1. **Contexto Histórico**:
 A função rand() foi introduzida na biblioteca padrão do C como a maneira mais simples de gerar números aleatórios. A função srand() foi adicionada posteriormente para permitir a inicialização do gerador com uma semente, normalmente o tempo atual.

2. **Alternativas**:
Existem algoritmos de geração de números aleatórios mais sofisticados disponíveis, como o algoritmo de Mersenne Twister, que gera números com uma distribuição uniforme. Contudo, para a maioria dos casos, rand() é suficiente.

3. **Detalhes de Implementação**:
A geração de números aleatórios em C é pseudoaleatória, ou seja, gerada por um algoritmo determinístico e, portanto, não é realmente aleatória. Usamos a função srand() para fornecer uma "semente" para o gerador de números aleatórios, para evitar gerar a mesma sequência de números em cada execução do programa.

## Veja Também:

1. Documentação rand() e srand() na referência da biblioteca C: https://en.cppreference.com/w/c/numeric/random/rand
2. Discutindo a função rand(): https://stackoverflow.com/questions/1653958/why-does-rand-yield-the-same-sequence-of-numbers-on-every-run
3. Documentação do Mersenne Twister: http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/ARTICLES/mt.pdf