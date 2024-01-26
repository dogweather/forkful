---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:48:40.115463-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Gerar números aleatórios é como rolar um dado virtual - você nunca sabe que número vai sair. Programadores usam isso para tudo, desde jogos até simulações e segurança de dados, dando aquele tempero de imprevisibilidade que certas situações pedem.

## How To:
Vamos lançar um dado usando o C++ moderno. Confira:

```C++
#include <iostream>
#include <random>

int main() {
    std::random_device rd; // Inicia a semente
    std::mt19937 gen(rd()); // Gera números com base na semente
    std::uniform_int_distribution<> distr(1, 6); // Define o intervalo de 1 a 6

    for(int n=0; n<10; ++n)
        std::cout << distr(gen) << ' '; // Lança o dado 10 vezes
    std::cout << std::endl;

    return 0;
}
```

Saída de exemplo (vai variar a cada execução):
```
4 2 3 5 6 3 1 4 2 5
```

## Deep Dive:
A geração de números aleatórios em C++ evoluiu bastante ao longo do tempo. A biblioteca `<random>` é relativamente nova, introduzida no C++11. Antes disso, a abordagem padrão era usar a função `rand()` da biblioteca `<cstdlib>`, mas ela tem várias limitações, como um intervalo menos flexível e menor qualidade na distribuição dos números.

A `<random>` trouxe geradores de números pseudoaleatórios mais sofisticados, como `std::mt19937`, conhecido como Mersenne Twister. Há também geradores para diferentes distribuições. No exemplo, usamos `std::uniform_int_distribution` para igual probabilidade, mas você poderia usar `std::normal_distribution` se precisasse de uma distribuição normal (em forma de sino).

Uma vantagem importante do Mersenne Twister é a sua periodicidade longa e qualidade dos números gerados. Porém, ele não é criptograficamente seguro. Para geração de números aleatórios seguros, outras bibliotecas ou algoritmos especializados são recomendados.

Uma nota adicional: mesmo com geradores sofisticados, a "randomicidade" está vinculada a qualidade da semente (`std::random_device` aqui). Uma semente ruim pode reduzir drasticamente a eficácia da geração de números aleatórios.

## See Also:

- Documentação oficial da biblioteca `<random>` do C++: http://www.cplusplus.com/reference/random/
- Um artigo detalhado sobre geração de números aleatórios em C++: https://en.cppreference.com/w/cpp/numeric/random
- Discussão sobre segurança na geração de números aleatórios: https://www.cryptopp.com/wiki/RandomNumberGenerator
