---
title:                "Gerando Números Aleatórios"
html_title:           "C: Gerando Números Aleatórios"
simple_title:         "Gerando Números Aleatórios"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O que & Por que?
Gerar números aleatórios é um processo de produzir números de forma imprevisível. Isso é útil para programadores que precisam de um elemento aleatório em seus programas, como em jogos ou sorteios.

## Como fazer:
```C 
#include <stdio.h> 
#include <stdlib.h> 
#include <time.h> 
  
int main () 
{ 
    // Definir a semente para gerar números aleatórios 
    srand(time(0)); 
  
    // Gerar um número aleatório entre 0 e 100 
    int numero = rand() % 101; 
  
    // Imprimir o número gerado 
    printf("O número aleatório gerado é: %d", numero); 
  
    return 0; 
} 
``` 
Saída: O número aleatório gerado é: 52

## Mergulho Profundo:
Gerar números aleatórios é uma prática que vem sendo utilizada desde os primórdios da computação. No início, os computadores não conseguiam produzir números aleatórios por conta própria, então era necessário utilizar ferramentas externas, como um lançamento de dados.

Hoje em dia, existem outras formas de gerar números aleatórios, como por exemplo, através de algoritmos que utilizam a hora atual do sistema como base. Porém, é importante ressaltar que esses números não são perfeitamente aleatórios, uma vez que podem ser previstos se o padrão for descoberto.

Para garantir maior segurança em aplicações que exigem números verdadeiramente aleatórios, é possível utilizar serviços externos que utilizam hardwares especializados, como geradores de ruído eletrônico, para gerar números aleatórios.

## Veja também:
Links para informações adicionais sobre geração de números aleatórios em C:
- [Biblioteca rand()](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Mersenne Twister - um algoritmo popular para geração de números aleatórios](https://en.wikipedia.org/wiki/Mersenne_Twister)
- [Biblioteca OpenSSL - opção para gerar números aleatórios criptograficamente seguros](https://www.openssl.org/)