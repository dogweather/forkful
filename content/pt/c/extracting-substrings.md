---
title:                "Extração de Substrings"
html_title:           "C: Extração de Substrings"
simple_title:         "Extração de Substrings"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## O que & Por que?

Extração de substrings é o processo de obter uma parte de uma sequência de caracteres maior. Isso é frequentemente feito por programadores para manipular ou analisar dados de maneira mais específica.

## Como:

Um exemplo simples de extração de substrings em C é usar a função `strncpy()` para copiar uma parte especificada de uma sequência de caracteres para uma nova variável. Veja o código abaixo:

```
#include <stdio.h>
#include <string.h>

int main() 
{
    char string[20] = "Olá Mundo!";
    char substring[8];
    
    strncpy(substring, string + 4, 7);
    
    printf("Substring: %s", substring);
    
    return 0;
}
```
Output:
```
Substring: Mundo!
```

## Profundando:

A extração de substrings tem sido usada desde os primeiros dias da programação de computadores, quando era necessário manipular dados em nível de caractere. No entanto, existem alternativas mais modernas, como expressões regulares, que podem ser mais poderosas e flexíveis para manipulação de texto.

A implementação da função `strncpy()` pode variar dependendo do compilador e das especificações da biblioteca string.h. Geralmente, ela extrai uma determinada quantidade de bytes da sequência de caracteres especificada e copia para uma nova variável. Porém, é importante ter cuidado com limites de memória e garantir que todos os caracteres sejam nulos-terminados para evitar erros.

## Veja também:

- [Documentação oficial da função strncpy()](https://www.tutorialspoint.com/c_standard_library/c_function_strncpy.htm)
- [Tutoriais de expressões regulares em C](https://www.geeksforgeeks.org/c-regex-examples/)
- [Outras funções úteis para manipulação de string em C](https://www.tutorialspoint.com/c_standard_library/string_h.htm)