---
title:                "Extraindo Substrings"
html_title:           "C: Extraindo Substrings"
simple_title:         "Extraindo Substrings"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que?

Extração de substrings é uma habilidade essencial em programação, especialmente no contexto de processamento de texto. Com ela, você pode criar adequar strings que contêm informações a outras partes do seu código, otimizar seus algoritmos e trabalhar com dados de forma mais eficiente.

## Como fazer:

Para extrair substrings em C, usamos a função `substring()` com os parâmetros de busca e tamanho desejados. Aqui está um exemplo de código que extrai os primeiros 5 caracteres de uma string e imprime o resultado:

```C
#include <stdio.h>

int main() {
    char *string = "Hello World";
    char *substring = substring(string, 0, 5);
    printf("%s\n", substring);
    // Output: Hello
}
```

Podemos também combinar as funções `substring()` e `strlen()` para extrair os últimos caracteres de uma string, como mostrado no exemplo abaixo:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char *string = "Hello World";
    int size = strlen(string);
    char *substring = substring(string, size - 5, 5);
    printf("%s\n", substring);
    // Output: World
}
```

## Mais aprofundamento:

A função `substring()` é definida na biblioteca `string.h` e podemos verificar sua implementação usando o comando `man` ou pesquisando na documentação online. Ela é uma função poderosa que permite extrair não apenas caracteres de uma string, mas também um intervalo específico ou até mesmo uma string de outro tipo de dado, como um inteiro.

## Veja também:

- [Documentação da função substring em C](https://www.tutorialspoint.com/c_standard_library/c_function_substr.htm)
- [Tutorial sobre extração de substrings em C](https://www.geeksforgeeks.org/string-functions-in-c-with-examples/#stringMan2)