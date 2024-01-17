---
title:                "Busca e substituição de texto"
html_title:           "C: Busca e substituição de texto"
simple_title:         "Busca e substituição de texto"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O que e Por que?

Substituir e buscar texto e uma tarefa frequente no mundo da programacao. Isso significa, como o nome sugere, encontrar determinado texto e substitui-lo por outro. Programadores fazem isso para corrigir erros, atualizar informacoes ou fazer alteracoes em massa em seus programas.

## Como fazer:

Existem varias maneiras de buscar e substituir texto em C. Uma delas e usando a funcao `str_replace()`, que recebe tres argumentos: a string original, o texto a ser buscado e o texto a ser substituido. Veja um exemplo:

```
#include <stdio.h>
#include <string.h>

int main() {
   char original[100] = "Ola mundo";
   char novo[100];
   str_replace(original, "mundo", "programacao", novo);
   printf("%s", novo);
   return 0;
}
```

Este codigo ira substituir o texto "mundo" por "programacao" e imprimir "Ola programacao" no terminal.

## Mergulho Profundo:

Buscar e substituir texto e uma funcao essencial em linguagens de programacao, e ja foi um processo manual no passado. Com o avanco da tecnologia, surgiram ferramentas e recursos que tornaram esse processo mais rapido e eficiente. Em C, a funcao `str_replace()` e uma alternativa mais simples e eficaz em comparacao com outras opcoes disponiveis. A linguagem tambem possui outras funcoes para buscar e substituir texto, como `strchr()` e `strtok()`, mas `str_replace()` e mais abrangente e facil de usar.

## Veja Tambem:

- [Documentacao oficial do C](https://www.gnu.org/software/libc/manual/html_node/String-Search-and-Replace.html)
- [Guia para iniciantes em C](https://guides.github.com/activities/hello-world/)