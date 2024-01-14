---
title:                "C++: Geração de números aleatórios"
simple_title:         "Geração de números aleatórios"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios em C++ é importante?

Gerar números aleatórios é uma tarefa comum em muitos programas de computador. Isso é útil para simular eventos aleatórios, selecionar valores aleatórios em um conjunto de dados ou até mesmo para criar senhas aleatórias. A linguagem de programação C++ oferece várias opções para gerar números aleatórios de forma eficiente e isso pode ser uma habilidade valiosa para qualquer programador.

## Como gerar números aleatórios em C++?

Em C++, existem algumas bibliotecas e funções que podem ser usadas para gerar números aleatórios. Uma delas é a função "rand()", que está disponível na biblioteca "stdlib.h". Esta função retorna um número inteiro aleatório dentro de um determinado intervalo. Por exemplo, se quisermos gerar um número aleatório entre 0 e 100, podemos usar o seguinte código:

```C++
#include <iostream>
#include <cstdlib>

int main() {
    // Gera um número aleatório entre 0 e 100
    int numero = rand() % 101;
    
    // Imprime o número gerado
    std::cout << "O número aleatório é: " << numero << std::endl;
    
    return 0;
}
```

Este pequeno programa irá gerar um número aleatório a cada vez que for executado. Além disso, existem outras opções, como a função "srand()" para especificar uma semente para a geração de números aleatórios ou a biblioteca "random" para gerar números mais precisos e aleatórios.

## Aprofundando-se na geração de números aleatórios em C++

Para entender melhor como a geração de números aleatórios funciona em C++, é importante saber que os computadores usam algoritmos para gerar números aleatórios. Esses algoritmos usam um "seed" (semente) para iniciar o processo e gerar números pseudorandomicos. Uma semente é um valor inicial que é usado para "iniciar" o algoritmo. O valor da semente é importante porque determina a sequência de números gerados. Isso significa que, se a mesma semente for usada novamente, a mesma sequência de números será gerada.

Uma das melhores práticas para gerar números aleatórios é usar a função "random_device" para gerar uma semente a partir de um dispositivo de hardware do computador, o que garante mais aleatoriedade.

## Veja também

- [Documentação da função "rand()" em C++](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Documentação da biblioteca "random" em C++](https://en.cppreference.com/w/cpp/numeric/random)