---
title:                "C++: Geração de números aleatórios"
programming_language: "C++"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios em C++?

Gerar números aleatórios pode ser útil em muitas situações de programação. Por exemplo, ao criar um jogo, é comum usar números aleatórios para gerar eventos imprevisíveis e tornar o jogo mais dinâmico. Além disso, gerar números aleatórios também é importante em simulações e algoritmos de criptografia.

## Como fazer em C++?

Para gerar números aleatórios em C++, é necessário incluir a biblioteca <random> e criar um objeto da classe `random_device`. Em seguida, é possível usar esta classe para gerar números aleatórios usando diferentes distribuições, como a distribuição uniforme ou a distribuição normal.

Um exemplo de código para gerar 5 números aleatórios entre 1 e 10 seria:

```C++
#include <iostream>
#include <random>

int main() {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(1, 10);

    for (int i = 0; i < 5; ++i) {
        std::cout << dis(gen) << " "; // imprime cada número gerado separado por espaço
    }
    std::cout << "\n";

    return 0;
}
```

O resultado deste código pode ser, por exemplo: `4 9 2 7 3`.


## Mergulho profundo

A geração de números aleatórios em C++ é feita usando algoritmos que produzem uma sequência de números aparentemente aleatórios. No entanto, estes algoritmos são baseados em uma semente inicial, que determina toda a sequência de números gerada. Por isso, é importante usar uma semente verdadeiramente aleatória para garantir que a sequência resultante seja realmente imprevisível.

Além disso, é possível gerar números pseudo-aleatórios em C++, que são determinísticos e repetíveis. Isso pode ser útil para testes e depuração, porém não deve ser usado para criptografia ou segurança.

## Veja também

- [Documentação da biblioteca <random> em C++](https://en.cppreference.com/w/cpp/numeric/random)
- [Exemplos de código para gerar números aleatórios em C++](https://www.tutorialspoint.com/generate-random-numbers-in-cplusplus)