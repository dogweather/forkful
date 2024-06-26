---
date: 2024-01-27 20:33:01.244547-07:00
description: "Como Fazer: Para gerar n\xFAmeros aleat\xF3rios em C++, voc\xEA normalmente\
  \ faria uso do cabe\xE7alho `<random>`, que foi introduzido no C++11, oferecendo\
  \ uma ampla\u2026"
lastmod: '2024-03-13T22:44:46.875545-06:00'
model: gpt-4-0125-preview
summary: "Para gerar n\xFAmeros aleat\xF3rios em C++, voc\xEA normalmente faria uso\
  \ do cabe\xE7alho `<random>`, que foi introduzido no C++11, oferecendo uma ampla\
  \ gama de facilidades para gerar n\xFAmeros aleat\xF3rios de v\xE1rias distribui\xE7\
  \xF5es."
title: "Gera\xE7\xE3o de n\xFAmeros aleat\xF3rios"
weight: 12
---

## Como Fazer:
Para gerar números aleatórios em C++, você normalmente faria uso do cabeçalho `<random>`, que foi introduzido no C++11, oferecendo uma ampla gama de facilidades para gerar números aleatórios de várias distribuições.

```C++
#include <iostream>
#include <random>

int main() {
    // Inicializa um motor de aleatoriedade
    std::random_device rd;  
    std::mt19937 gen(rd()); 

    // Define o intervalo [0, 99] inclusivo
    std::uniform_int_distribution<> distrib(0, 99); 

    // Gera e imprime 5 números aleatórios dentro do intervalo definido
    for(int n=0; n<5; ++n)
        std::cout << distrib(gen) << ' ';
    return 0;
}
```

Este exemplo de código inicializa um gerador de números aleatórios Mersenne Twister com uma semente de `std::random_device`. Em seguida, define uma distribuição uniforme de inteiros no intervalo [0, 99] e, finalmente, imprime 5 números aleatórios dessa distribuição.

A saída de exemplo pode parecer assim, mas tenha em mente que cada execução provavelmente produzirá resultados diferentes:

```
45 67 32 23 88
```

## Aprofundando:
Historicamente, a geração de números aleatórios em C++ dependia fortemente da função `rand()` e da função de semeadura `srand()`, encontradas no cabeçalho `<cstdlib>`. No entanto, essa abordagem frequentemente enfrentava críticas pela falta de uniformidade e previsibilidade na distribuição dos números gerados.

A introdução do cabeçalho `<random>` no C++11 marcou uma melhoria significativa, oferecendo um sistema sofisticado para produzir números aleatórios. As facilidades fornecidas incluem uma variedade de motores (como `std::mt19937` para Mersenne Twister) e distribuições (como `std::uniform_int_distribution` para distribuição uniforme de inteiros) que podem ser combinadas para atender às necessidades específicas do programador, levando a um comportamento mais previsível, melhor desempenho e maior flexibilidade.

Embora a biblioteca `<random>` seja muito melhor do que a abordagem antiga `rand()`, vale ressaltar que gerar números verdadeiramente aleatórios — especialmente para fins criptográficos — ainda depende de considerações adicionais. Para aplicações criptográficas, devem ser usadas bibliotecas projetadas especificamente para segurança, que muitas vezes utilizam fontes de entropia de hardware.
