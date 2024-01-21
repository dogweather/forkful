---
title:                "Concatenando strings"
date:                  2024-01-20T17:34:21.700954-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenando strings"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Concatenar strings é o processo de juntar duas ou mais sequências de caracteres para formar um novo texto. Programadores fazem isso para construir mensagens dinâmicas, processar entradas de dados ou simplesmente para montar informações em formatos específicos durante a escrita de um software.

## Como Fazer:

A seguir, exemplos de código que demonstram como concatenar strings em C:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char primeiraString[] = "Olá, ";
    char segundaString[] = "mundo!";
    char stringConcatenada[50];

    strcpy(stringConcatenada, primeiraString); // Inicia com a primeira string.
    strcat(stringConcatenada, segundaString); // Adiciona a segunda string.

    printf("%s\n", stringConcatenada); // Mostra o resultado.

    return 0;
}
```

Saída:
```
Olá, mundo!
```

## Mergulho Profundo

Concatenar strings em C não é tão direto quanto em linguagens de alto nível, que possuem operadores de concatenação. Em C, precisa-se utilizar funções da biblioteca `string.h`, tais como `strcpy` e `strcat`. Historicamente, manipulação de strings em C sempre foi um pouco mais 'manual'.

Alternativas incluem a utilização de funções mais seguras, como `strncat` ou `snprintf`, para evitar casos de estouro de buffer, que podem levar a vulnerabilidades de segurança. Outro detalhe é que o array que receberá a string concatenada precisa ser grande o suficiente para acomodar a nova string — um aspecto fundamental de programação em C.

Quando uma performance superior é necessária, outras técnicas como a construção de strings com funções de baixo nível (ex., `memcpy`) podem ser adotadas para otimizar o processo.

## Veja Também

- Documentação do GCC (GNU Compiler Collection), para verificar quaisquer atualizações na manipulação de strings em C: https://gcc.gnu.org/onlinedocs/
- Manual do POSIX `string.h`, para uma melhor compreensão das funções de strings disponíveis em sistemas compatíveis com POSIX: https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/string.h.html
- Tutorial de C da cplusplus.com, um bom recurso para aprender mais sobre manipulação de strings: http://www.cplusplus.com/reference/cstring/