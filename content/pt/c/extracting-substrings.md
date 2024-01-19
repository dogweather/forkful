---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?

Extrair substring é a ação de selecionar um grupo de caracteres contínuos de uma string (texto) maior - tipo pegar um pedaço de um bolo. Programadores fazem isso pra simplificar e otimizar o tratamento de dados textuais.

## Como Fazer:

Para exemplificar, vamos usar a função `strncpy` de `string.h` que copia uma quantidade específica de caracteres de uma string pra outra:

```C
#include <stdio.h>
#include <string.h>

int main() {
  char texto[13] = "bom dia mundo";
  char substring[5];
  
  strncpy(substring, &texto[4], 4);
  substring[4] = '\0';

  printf("%s\n", substring);
  return 0;
}
```

Nesse exemplo, a saída será `dia `.

## Deep Dive:

A estratégia de extração de substrings vem da época dos primeiros sistemas de computação, quando o armazenamento de dados era limitado e caro - por isso era importante manusear apenas o necessário.

Alternativamente, pode-se usar a função `sscanf`, que tem mais flexibilidade mas exige mais memória. Aqui também é válido atentar que a função `strncpy` não adiciona um nulo ('\0') ao final da nova string, então é importante não esquecer de inserir manualmente pra evitar problemas.

O jeitinho de extrair substrings pode variar de linguagem pra linguagem. No C, os dados de strings são manipulados diretamente na memória, o que deixa o código rápido, mas pede cuidado extra.

## Veja Também:

Dá uma olhada no manual das funções pra entender melhor [strncpy()](https://www.cplusplus.com/reference/cstring/strncpy/?kw=strncpy) e [sscanf()](https://www.cplusplus.com/reference/cstdio/sscanf/). E aqui o artigo sobre [string handling](https://www.learn-c.org/en/String_Handling) no C.