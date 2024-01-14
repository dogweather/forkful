---
title:    "C: Gerando números aleatórios"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por que gerar números aleatórios pode ser útil na programação?

Gerar números aleatórios é uma tarefa comum e útil na programação. Isso permite que os programadores criem programas com comportamentos imprevisíveis ou variados, o que pode ser útil em jogos, simulações, testes e outras aplicações.

## Como gerar números aleatórios em C

Os números aleatórios em C são gerados usando a função `rand()`, que retorna um número inteiro pseudoaleatório entre 0 e `RAND_MAX`, uma constante definida na biblioteca `stdlib.h`. Para usar a função `rand()` é necessário incluir a biblioteca `stdlib.h` no início do seu programa. Veja um exemplo abaixo:

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
    // Gerando um número aleatório entre 0 e 9
    int numero = rand() % 10;

    printf("O número aleatório gerado é: %d\n", numero);

    return 0;
}
```

A saída desse código pode variar a cada vez que o programa é executado, pois o número gerado é pseudoaleatório. Para gerar números aleatórios diferentes a cada execução, é possível usar a função `srand()` para inicializar a semente do gerador de números aleatórios. Veja um exemplo abaixo:

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
    // Inicializando a semente com o valor do relógio do sistema
    srand(time(NULL));

    // Gerando um número aleatório entre 0 e 9
    int numero = rand() % 10;

    printf("O número aleatório gerado é: %d\n", numero);

    return 0;
}
```

Nesse caso, o valor do relógio do sistema é usado como semente para o gerador de números aleatórios, o que garante que a sequência de números gerada seja diferente a cada execução.

## Mais informações sobre geração de números aleatórios

Existem diversas técnicas e algoritmos para geração de números aleatórios, e a maioria deles se baseia em alguma forma de cálculo matemático. Porém, é importante lembrar que todos esses algoritmos geram números pseudoaleatórios, ou seja, sequências de números que se parecem aleatórios mas na verdade são determinísticos. Isso significa que a sequência de números gerada pode ser reproduzida se a mesma semente for usada.

Além disso, é importante tomar cuidado ao usar números gerados aleatoriamente em situações que exijam alta segurança, pois esses números podem ser previsíveis e suscetíveis a ataques. Em casos como esse, é recomendado usar geradores de números aleatórios criptograficamente seguros, que usam fontes externas de entropia para aumentar a imprevisibilidade dos números gerados.

## Veja também

- [Função `rand()` na documentação do C](https://en.cppreference.com/w/c/numeric/random/rand)
- [Geradores de números aleatórios criptograficamente seguros em C](https://www.geeksforgeeks.org/generating-random-number-in-cc/)
- [Artigo sobre geração de números aleatórios](https://www.sciencedirect.com/topics/computer-science/random-number-generation)