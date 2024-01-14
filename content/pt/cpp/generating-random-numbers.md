---
title:    "C++: Gerando números aleatórios"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por que gerar números aleatórios é importante?

Gerar números aleatórios é uma habilidade essencial em programação, pois muitas vezes precisamos de valores imprevisíveis ou variáveis para testes e simulações. É importante saber como gerar esses números de forma eficiente para melhorar a qualidade e precisão do seu código.

## Como gerar números aleatórios em C++?

Para gerar números aleatórios em C++, podemos utilizar a biblioteca padrão ```<random>```. Primeiramente, devemos incluir essa biblioteca em nosso código, utilizando o comando ```#include <random>```.

Em seguida, precisamos definir um gerador de números aleatórios, que pode ser do tipo ```minstd_rand0``` ou ```mt19937```. Podemos criar esse gerador utilizando o namespace ```std::random_device```, que fornece um valor inicial para o gerador.

Agora, podemos utilizar o gerador para gerar números aleatórios utilizando o método ```operator()```. Por exemplo, para gerar um número inteiro aleatório entre 1 e 10, podemos utilizar o seguinte código:

```C++
std::random_device rd; // gerador de números aleatórios
std::mt19937 rng(rd()); // inicializa o gerador
std::uniform_int_distribution<int> dist(1, 10); // define a distribuição de números
int aleatorio = dist(rng); // gera um número aleatório
```

Podemos também gerar números aleatórios de outros tipos, como ponto flutuante ou caracteres ASCII, utilizando diferentes distribuições fornecidas pela biblioteca ```<random>```.

## Aprofundando-se em gerar números aleatórios

Além da biblioteca ```<random>``` do C++, existem outras formas de gerar números aleatórios, como a utilização da biblioteca externa Boost.Random ou a geração de números pseudoaleatórios utilizando funções matemáticas como a função ```rand()```.

Além disso, é importante entender que a geração de números aleatórios não é realmente aleatória, mas sim determinística. Isso significa que, dada uma mesma semente (valor inicial), um gerador de números aleatórios sempre irá produzir a mesma sequência de números.

Por isso, é importante escolher cuidadosamente a semente para obter uma boa variedade de números aleatórios. Além disso, é possível utilizar algoritmos de embaralhamento para produzir uma sequência de números mais variada e imprevisível.

## Veja também
- [Documentação oficial da biblioteca <random> do C++](https://en.cppreference.com/w/cpp/numeric/random)
- [Gerando números aleatórios com a biblioteca Boost](https://www.boost.org/doc/libs/1_77_0/doc/html/boost_random.html)
- [A função rand() em C++](https://www.cplusplus.com/reference/cstdlib/rand/)