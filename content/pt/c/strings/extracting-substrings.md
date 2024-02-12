---
title:                "Extraindo substrings"
aliases:
- /pt/c/extracting-substrings/
date:                  2024-02-03T17:56:26.414946-07:00
model:                 gpt-4-0125-preview
simple_title:         "Extraindo substrings"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/extracting-substrings.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O quê & Por quê?

Extrair substrings em C envolve criar uma string menor (substring) a partir de uma string maior com base em critérios especificados, como posição e comprimento. Programadores frequentemente realizam essa tarefa para análise de texto, processamento de dados ou validação de entrada, tornando-a uma habilidade crucial na manipulação e análise eficiente de dados de texto.

## Como fazer:

Ao contrário de algumas linguagens de alto nível que fornecem métodos embutidos para a extração de substrings, o C requer uma abordagem mais manual usando suas funções de manipulação de strings. Aqui está como extrair efetivamente uma substring em C:

### Exemplo 1: Usando `strncpy`

```c
#include <stdio.h>
#include <string.h>

int main() {
    char texto[] = "Hello, World!";
    char buffer[20];

    // Extrair "World" de "Hello, World!"
    strncpy(buffer, texto + 7, 5);
    buffer[5] = '\0'; // Garantir terminação nula

    printf("Substring extraída: %s\n", buffer);
    // Saída: Substring extraída: World
    return 0;
}
```

### Exemplo 2: Criando uma Função

Para uso repetido, uma função dedicada para extrair substrings pode ser mais eficiente:

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void extractSubstring(char *origem, int de, int n, char *destino) {
    strncpy(destino, origem + de, n);
    destino[n] = '\0'; // Garantir terminação nula
}

int main() {
    char texto[] = "Programming in C";
    char buffer[50];

    extractSubstring(texto, 0, 11, buffer);
    printf("Substring extraída: %s\n", buffer);
    // Saída: Substring extraída: Programming
    return 0;
}
```

## Aprofundamento

Extrair substrings em C é primariamente tratado através da manipulação de ponteiros e gerenciamento cuidadoso da memória, refletindo a abordagem de baixo nível do idioma para manipulação de dados. Este método remonta aos primeiros dias da programação em C, quando o gerenciamento de recursos de forma eficiente era primordial devido ao poder de computação limitado. Enquanto a ausência de uma função embutida para substrings pode parecer uma omissão, ela exemplifica a filosofia do C de dar aos programadores controle total sobre o gerenciamento de memória, muitas vezes levando a um código otimizado mas mais complexo.

No mundo da programação moderna, linguagens como Python e JavaScript oferecem métodos embutidos para extração de substrings, como `slice()` ou fatiamento de string usando índices. Essas linguagens de alto nível lidam com o gerenciamento de memória por trás dos panos, trocando algum grau de controle por facilidade de uso e legibilidade.

Para programadores em C, entender aritmética de ponteiros e alocação de memória é vital para tarefas como extração de substrings. Enquanto essa abordagem exige uma compreensão mais profunda de como as strings são representadas e manipuladas na memória, ela oferece controle e eficiência sem paralelo, características marcantes da programação em C que mantiveram sua relevância em aplicações críticas ao desempenho por décadas. No entanto, para aqueles trabalhando em aplicações de alto nível onde o gerenciamento direto da memória é menos uma preocupação, linguagens com funcionalidades de substring embutidas podem oferecer uma abordagem mais simples e menos propensa a erros.
