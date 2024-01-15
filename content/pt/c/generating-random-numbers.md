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

## Por que gerar números aleatórios?

Gerar números aleatórios é uma tarefa extremamente comum em programação. Esses números são úteis em diversas situações, como jogos, simulações, criptografia e testes de desempenho. Além disso, muitos algoritmos e estruturas de dados se baseiam na aleatoriedade para obter resultados mais eficientes.

## Como fazer isso em C

A geração de números aleatórios em C é feita pela função `rand()`, presente na biblioteca padrão `stdlib.h`. Essa função retorna um número inteiro aleatório entre 0 e `RAND_MAX`, uma constante definida na mesma biblioteca. Porém, para que os números gerados sejam realmente imprevisíveis, é necessário inicializar a semente da função `rand()` utilizando a função `srand()`. Essa semente pode ser definida de diferentes maneiras, como por exemplo utilizando o valor do horário atual com a função `time()`.

Veja um exemplo simples de como gerar 10 números aleatórios entre 1 e 100:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(void)
{
    srand(time(NULL)); // inicializa a semente da função rand()
    
    for (int i = 0; i < 10; i++)
    {
        int num = rand() % 100 + 1; // gera um número aleatório entre 1 e 100
        printf("%d ", num); // imprime o número gerado
    }
    
    return 0;
}
```

Exemplo de saída:

`29 46 79 13 62 89 5 92 48 32`

## Mais detalhes sobre geração de números aleatórios

A função `rand()` não gera números realmente aleatórios, mas sim pseudoaleatórios. Isso significa que, para uma mesma semente, ela sempre irá gerar a mesma sequência de números. Por isso é importante utilizar uma semente diferente a cada vez que o programa é executado.

Além disso, a função `rand()` utiliza um algoritmo determinístico para gerar os números, o que pode levar a padrões previsíveis em certas situações. Para uma geração mais precisa, existem outras técnicas como os geradores de números pseudoaleatórios baseados em ruído ou em algoritmos criptográficos.

## Veja também

- [Documentação oficial do C](https://devdocs.io/c/)
- [Geração de números aleatórios em C++](https://medium.com/@justinbrown761/generating-random-numbers-in-c-c-using-standard-libraries-dd1103b716ec)
- [Artigo sobre geradores de números pseudoaleatórios](https://www.sciencedirect.com/science/article/pii/S0169409X01000011)