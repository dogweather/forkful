---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que & Por quê?

Gerar números aleatórios em programação é a criação de números que não podem ser previsivelmente determinados. Os programadores fazem isso para criar cenários de teste, simulações, jogos, algoritmos de criptografia entre outros.

## Como fazer:

Aqui está um exemplo básico de como gerar um número aleatório entre 1 e 10 em C++.
```C++
#include <iostream>
#include <cstdlib> 
#include <ctime> 

int main() {
    std::srand(std::time(0)); 
    int numAleatorio = std::rand() % 10 + 1;
    std::cout << "Número aleatório: " << numAleatorio;
    return 0;
}
```
Quando você executa este código, este produzirá um número aleatório entre 1 e 10 na saída.

## Aprofundando

Os números gerados por `std::rand()` são considerados "pseudoaleatórios" pois dependem do valor inicial (`std::time(0)`). Isso foi introduzido nos anos 70, e ainda é usado hoje, mas tem suas limitações.

Um método mais moderno em C++11 é usar a biblioteca `<random>`. Ela fornece uma série de geradores de números pseudoaleatórios que são muito melhores do que o `std::rand()`.

```C++
#include <random>
#include <iostream>

int main() {
    std::random_device rd; 
    std::mt19937 gen(rd()); 
    std::uniform_int_distribution<> dis(1, 10);

    for (int n=0; n<10; n++) {
        std::cout << dis(gen) << ' ';
    }
    return 0;
}
```
Cada execução desse código produzirá uma sequência diferente de 10 números inteiros aleatórios entre 1 e 10.

## Veja Também

Para obter mais informações sobre a geração de números aleatórios em C++, consulte os links a seguir:
- Biblioteca C++ `<random>`: [www.cplusplus.com/reference/random](http://www.cplusplus.com/reference/random/)
- `std::rand()`: [www.cplusplus.com/reference/cstdlib/rand](http://www.cplusplus.com/reference/cstdlib/rand/)
- Gerando números aleatórios em C++: [www.geeksforgeeks.org](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)