---
title:                "Extraindo substrings"
date:                  2024-01-20T17:45:07.704512-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extraindo substrings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Extrair substrings significa pegar partes específicas de uma string — como cortar um pedaço de uma fita ao tamanho que precisamos. Programadores fazem isso para manipular e analisar dados de texto de forma mais precisa e eficiente.

## Como Fazer:
Para extrair uma substring em C, geralmente usamos a função `strncpy`. Atenção para adicionar o caractere nulo no final.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char texto[] = "Programar é arte!";
    char sub[7]; // Substring + caractere nulo

    strncpy(sub, texto + 5, 6); // Copia 6 caracteres a partir da posição 5
    sub[6] = '\0'; // Não esqueça do caractere nulo

    printf("Substring: %s\n", sub);

    return 0;
}
```
Saída:
```
Substring: amar é
```

## Deep Dive
Extrair substrings é uma operação básica, mas essencial. Originou-se na manipulação de strings nas primeiras linguagens de programação e persiste até hoje devido à sua utilidade. Existem várias alternativas para fazer isso em C. Além de `strncpy`, funções como `sscanf` e `strchr` podem ser utilizadas para casos específicos. Implementar sua própria lógica de extração pode ser necessário quando você busca eficiência e um controle fino sobre o gerenciamento de memória.

## Ver Também
Para aprofundar seu conhecimento sobre manipulação de strings em C, confira:
- C Standard Library: https://en.cppreference.com/w/c/string/byte
- Programação C Moderna com Técnicas Avançadas: https://pt.wikibooks.org/wiki/Programar_em_C
- Manipulação de strings: https://www.cprogramming.com/tutorial/c/lesson9.html
