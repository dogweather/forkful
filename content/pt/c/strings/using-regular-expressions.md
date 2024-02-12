---
title:                "Usando expressões regulares"
date:                  2024-02-03T18:10:46.961052-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando expressões regulares"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Expressões regulares (regex) fornecem uma maneira de buscar, combinar e manipular strings usando padrões definidos. Programadores as utilizam extensivamente para tarefas como validar entradas, analisar dados de texto e encontrar padrões dentro de grandes arquivos de texto, tornando-as uma ferramenta poderosa em qualquer linguagem, incluindo C.

## Como fazer:

Para usar expressões regulares em C, você trabalhará principalmente com a biblioteca regex POSIX (`<regex.h>`). Este exemplo demonstra correspondência de padrões básica:

```c
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

int main(){
    regex_t regex;
    int return_value;
    char *pattern = "^a[[:alnum:]]"; // Padrão para combinar strings que começam com 'a' seguido por caracteres alfanuméricos
    char *test_string = "apple123";

    // Compilar a expressão regular
    return_value = regcomp(&regex, pattern, REG_EXTENDED);
    if (return_value) {
        printf("Não foi possível compilar o regex\n");
        exit(1);
    }

    // Executar a expressão regular
    return_value = regexec(&regex, test_string, 0, NULL, 0);
    if (!return_value) {
        printf("Combinação encontrada\n");
    } else if (return_value == REG_NOMATCH) {
        printf("Nenhuma combinação encontrada\n");
    } else {
        printf("Falha na combinação do regex\n");
        exit(1);
    }

    // Liberar a memória alocada pelo regex
    regfree(&regex);

    return 0;
}
```

Saída de amostra para uma string que combina ("apple123"):
```
Combinação encontrada
```
E para uma string que não combina ("banana"):
```
Nenhuma combinação encontrada
```

## Aprofundamento:

Expressões regulares em C, como parte do padrão POSIX, oferecem uma maneira robusta de realizar combinação e manipulação de strings. Contudo, a API da biblioteca regex POSIX em C é considerada mais complicada do que aquelas encontradas em linguagens projetadas com recursos de manipulação de strings de primeira classe, como Python ou Perl. A sintaxe para padrões é semelhante entre as linguagens, mas C requer gerenciamento manual de memória e mais código boilerplate para preparar, executar e limpar após o uso de padrões regex.

Apesar desses desafios, aprender a usar regex em C é gratificante, pois aprofunda o entendimento de conceitos de programação de baixo nível. Além disso, abre possibilidades para programação em C em áreas como processamento de texto e extração de dados onde regex é indispensável. Para padrões mais complexos ou operações regex, alternativas como a biblioteca PCRE (Perl Compatible Regular Expressions) podem oferecer uma interface mais rica em recursos e um pouco mais fácil, embora requeira a integração de uma biblioteca externa ao seu projeto C.
