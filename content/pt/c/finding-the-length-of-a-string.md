---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?

Encontrar o comprimento de uma string em C é determinar o número de caracteres existentes numa string. É fundamental em programação para manipular e analisar dados e variáveis ​​de string corretamente.

## Como fazer:

Aqui está o código C para medir o comprimento de uma string usando a função `strlen()`, fornecida na biblioteca `string.h`:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Programação em C";
    int length;

    length = strlen(str);

    printf("Comprimento da string = %d\n", length);

    return 0;
}
```

A saída deste código será:

```
Comprimento da string = 17
```

## Aprofundando

A função `strlen()` foi adicionada na linguagem de programação C com a biblioteca padrão ANSI C, em 1989. Antes disso, programadores C teriam que write their own loops to manually count the length of a string.

A alternativa para `strlen()` é escrever um loop manualmente que percorre a string até alcançar o caractere nulo de terminação (`'\0'`). No entanto, `strlen()` é geralmente preferido por suas melhorias de desempenho e práticas recomendadas de código.

A função `strlen()` segue um princípio simples: ela começa no endereço de memória passado a ela e continua avançando até encontrar um byte zero, retornando o número de passes pelo loop - 1.

## Veja Também:

- A função strlen() na biblioteca C: http://www.cplusplus.com/reference/cstring/strlen/
- Contando o comprimento da string sem usar a função strlen: https://www.geeksforgeeks.org/c-program-to-find-length-of-a-string-without-using-strlen/