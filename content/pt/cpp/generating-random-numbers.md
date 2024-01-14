---
title:    "C++: Gerando números aleatórios"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios é importante?

Gerar números aleatórios é uma técnica crucial na programação. Com ele, podemos simular situações aleatórias, criar jogos e até mesmo garantir a segurança de algumas aplicações. É uma ferramenta poderosa que pode trazer muitos benefícios para o seu código.

## Como gerar números aleatórios em C++?

Para gerar números aleatórios em C++, podemos utilizar a biblioteca padrão `<random>`. Com ela, temos acesso a diversas funções e classes que facilitam o processo de geração de números aleatórios. Vamos ver um exemplo de código que gera 5 números aleatórios entre 0 e 100:

```C++
#include <iostream>
#include <random>

int main() {
    // Inicializa o gerador de números aleatórios com uma semente
    std::random_device rd;

    // Cria um motor de números aleatórios utilizando a semente
    std::mt19937 gen(rd());

    // Define o intervalo de geração de números
    std::uniform_int_distribution<> dist(0, 100);

    // Gera 5 números aleatórios e os imprime
    for (int i = 0; i < 5; i++) {
        int num = dist(gen);
        std::cout << num << std::endl;
    }

    return 0;
}
```

Exemplo de saída:  
34  
79  
16  
85  
2 

Nesse código, utilizamos a classe `std::mt19937` como o gerador de números aleatórios e `std::uniform_int_distribution` para definir o intervalo em que queremos gerar os números. Assim, podemos utilizar essas classes para controlar de forma mais precisa os números que queremos gerar.

## Profundidade na geração de números aleatórios

Por trás dos geradores de números aleatórios, existe uma teoria matemática complexa que é fundamental para garantir a "aleatoriedade" dos números gerados. Um dos métodos mais utilizados é o Método dos Quadrados Médios, que consiste em elevar o número gerado ao quadrado e utilizar os dígitos intermediários como a nova semente para a próxima geração de números. Dessa forma, os números gerados apresentam um bom nível de imprevisibilidade.

## Veja também

- [Documentação da biblioteca <random> do C++](https://docs.microsoft.com/pt-br/cpp/standard-library/random)
- [Como usar o método dos quadrados médios para gerar números aleatórios](https://pt.wikipedia.org/wiki/Quadrados_m%C3%A9dios)