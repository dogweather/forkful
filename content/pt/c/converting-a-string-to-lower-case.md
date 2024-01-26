---
title:                "Convertendo uma string para minúsculas"
date:                  2024-01-20T17:37:49.371209-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Converter uma string para minúsculas significa transformar todos os caracteres alfabéticos de uma string em letras minúsculas. Programadores fazem isso para padronizar inputs, facilitar comparações e buscas de strings, independentemente da capitalização original.

## Como Fazer:
Aqui está um exemplo de como converter uma string para minúsculas em C usando a função `tolower()` da biblioteca padrão:

```c
#include <stdio.h>
#include <ctype.h>

void strToLower(char *str) {
    while(*str) {
        *str = tolower((unsigned char) *str);
        str++;
    }
}

int main() {
    char myString[] = "Olá, Mundo!";
    printf("Original: %s\n", myString);
    strToLower(myString);
    printf("Minúsculas: %s\n", myString);
    return 0;
}
```

Saída esperada:
```
Original: Olá, Mundo!
Minúsculas: olá, mundo!
```

## Mergulho Profundo:
Historicamente, a necessidade de converter strings para minúsculas surgiu com a evolução dos sistemas de computador e a necessidade de processar texto de forma eficiente. Hoje, funções como `tolower()` fazem parte da biblioteca padrão C (stdlib.h), simplificando essa operação.

Existem alternativas para a função `tolower()`, como o uso de operações de máscara de bits em sistemas onde a tabela de caracteres segue a codificação ASCII. Porém, tais métodos são menos portáveis e ignoram localizações e caracteres especiais.

A implementação de funções como `tolower()` leva em conta a tabela de caracteres do sistema. Por isso, aplicativos C modernos devem preferir essas funções padrão, pois elas lidam com questões de localização e podem ser atualizadas para suportar novos padrões e idiomas.

## Veja Também:
Para mais informações e uma compreensão mais profunda de como manipular texto em C, visite os seguintes links:
- Documentação da função `tolower()`: https://en.cppreference.com/w/c/string/byte/tolower
- Tutorial GNU C Library sobre caracteres: https://www.gnu.org/software/libc/manual/html_node/Character-Handling.html
- Tutorial sobre manipulação de strings em C: https://www.tutorialspoint.com/cprogramming/c_strings.htm
