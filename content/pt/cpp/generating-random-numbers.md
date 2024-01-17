---
title:                "Geração de números aleatórios"
html_title:           "C++: Geração de números aleatórios"
simple_title:         "Geração de números aleatórios"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# O que e por que?

Gerar números aleatórios é o processo de criar valores numéricos sem nenhum padrão aparente. Os programadores usam esse recurso para adicionar elementos de imprevisibilidade e aleatoriedade em seus programas, o que pode ser útil em jogos, simulações e criptografia.

# Como fazer:

Existem várias maneiras de gerar números aleatórios em C++. Uma opção é usar a função ```rand()``` da biblioteca ```<cstdlib>```, que retorna um número inteiro aleatório. Por exemplo:

```C++
#include <iostream>
#include <cstdlib>
using namespace std;

int main() {
    // Gera um número aleatório entre 1 e 100
    int num = rand() % 100 + 1;
    cout << "Número aleatório: " << num << endl;
    return 0;
}
```

Este código irá imprimir um número aleatório na tela toda vez que for executado.

# Deep Dive:

Historicamente, a geração de números aleatórios em computadores era feita por meio de algoritmos determinísticos, ou seja, algoritmos que seguem um conjunto de regras definidas para gerar uma sequência de números. No entanto, esses números não são verdadeiramente aleatórios, pois o resultado pode ser previsto se o algoritmo e sua semente (um número inicial usado no cálculo) forem conhecidos.

Uma alternativa para gerar números verdadeiramente aleatórios é o uso de fontes de entropia, como ruído ambiental ou dados de movimento do mouse, combinados com algoritmos que os transformam em números aleatórios. Isso é conhecido como geração de números pseudo-aleatórios.

Além disso, é importante mencionar que alguns algoritmos de geração de números aleatórios podem produzir resultados tendenciosos ou até mesmo repetir a mesma sequência após um determinado número de iterações. Por esse motivo, é importante que os programadores se atentem à qualidade e propriedades estatísticas dos números gerados.

# Veja também:

- [Documentação sobre a função ```rand()```](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Artigo sobre geração de números aleatórios em C++](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [Article on generating random numbers in C++](https://www.geeksforgeeks.org/generating-random-number-range-c/) (em inglês)