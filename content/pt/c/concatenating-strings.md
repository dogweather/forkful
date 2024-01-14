---
title:    "C: Concatenação de Strings"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que concatenar strings em C?

A concatenação de strings em C é uma operação importante e comum na programação. Ela é utilizada para unir duas ou mais strings em uma única string maior. Isso é útil para criar mensagens personalizadas, formatar dados e muito mais.

## Como fazer a concatenação de strings em C?

Existem várias maneiras de concatenar strings em C, mas a forma mais simples é usando a função `strcat()`. Ela aceita dois parâmetros: a string de destino (onde as strings serão concatenadas) e a string de origem (a string que será adicionada à string de destino). Aqui está um exemplo de como usar a função `strcat()` em um programa:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str1[50] = "Olá, ";
    char str2[20] = "mundo!";
    strcat(str1, str2);
    printf("%s\n", str1);
    return 0;
}
```

Esse programa irá imprimir "Olá, mundo!" na tela. Primeiro, declaramos duas strings: `str1`, que já possui um valor, e `str2`, que será anexada a `str1`. Então, usamos a função `strcat()` para concatenar as duas strings, e o resultado é armazenado novamente em `str1`. Por fim, imprimimos `str1` na tela usando a função `printf()`.

## Aprofundando na concatenação de strings

Em C, as strings são arrays de caracteres terminados com o caractere `'\0'`, conhecido como "nulo". A função `strcat()` funciona movendo o ponteiro de destino para o final de `str1` e adicionando cada caractere de `str2` um por vez até encontrar o caractere nulo. Por isso é importante que a string de destino tenha espaço suficiente para armazenar o resultado da concatenação.

Outra função útil para concatenar strings é a `strncat()`, que também aceita dois parâmetros, mas adiciona apenas um número específico de caracteres da string de origem à string de destino. Isso pode ser útil em situações em que você precisa concatenar apenas parte de uma string.

## Veja também

Aqui estão alguns links que podem ajudar a aprofundar seu conhecimento sobre concatenação de strings em C:

- [Documentação da função `strcat()` em cplusplus.com](http://www.cplusplus.com/reference/cstring/strcat/)
- [Tutorial sobre strings em C no tutorialspoint.com](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Artigo sobre concatenação de strings em C no geeksforgeeks.org](https://www.geeksforgeeks.org/concatenate-strings-c-3-different-ways/)