---
title:                "Usando expressões regulares"
html_title:           "Gleam: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O quê & Por quê?

Regular Expressions (ou regex) são padrões usados para encontrar, substituir ou manipular texto. Programadores as utilizam para lidar eficientemente com strings e simplificar operações complexas de busca e substituição.

## Como fazer:

Para usar regex em C, precisamos incluir a biblioteca `<regex.h>`. Aqui está um exemplo básico para ilustrar como usar regex em C.

```C
#include <regex.h>
#include <stdio.h>

int main() {
    regex_t regex;
    int retorno;
   
    retorno = regcomp(&regex, "p", 0);
    retorno = regexec(&regex, "programacao", 0, NULL, 0);

    if (retorno == 0) {
        printf("Padrão encontrado.\n");
    } else {
        printf("Padrão não encontrado.\n");
    }
   
    regfree(&regex);
    return 0;
}
```
Este código verifica se a letra "p" está na palavra "programacao". Se estiver, ele imprimirá "Padrão encontrado.".

## Mergulho profundo

Regex tem uma longa história, com sua primeira aparição em ferramentas de processamento de texto da década de 1960. Ela é suportada em quase todas as modernas linguagens de programação e é uma ferramenta inestimável para manipular strings e texto.

Há alternativas para regex, como o uso de funções inbuilt para busca e substituição de strings, mas estas não têm a mesma flexibilidade e poder das expressões regulares.

Em C, a implementação de regex é feita através da biblioteca `<regex.h>`. Esta biblioteca fornece funções como `regcomp()` para compilar um padrão regex e `regexec()` para executar a expressão regular.

## Veja também

Consulte esses links para saber mais sobre expressões regulares em C:

1. [Documentação oficial de `<regex.h>`](http://man7.org/linux/man-pages/man3/regex.3.html)
2. [Tutorial interativo de Regex em Português](https://regexone.com/)
3. [Tutorial de Regex em C](https://www.lemoda.net/c/c-regex/)