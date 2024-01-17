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

## O que e por que?

Encontrar o comprimento de uma string é uma tarefa comum em programação. Isso significa determinar o número de caracteres presentes em uma string. Os programadores fazem isso para realizar diversas operações, como manipulação de strings, alocação de memória, ou para garantir que a string atende aos requisitos de tamanho.

## Como fazer:

Para encontrar o comprimento de uma string em C, podemos usar a função ```strlen()```, que está presente no header `<string.h>`. Essa função recebe a string como parâmetro e retorna um inteiro que representa o seu comprimento. Veja um exemplo de como usá-la:

```
#include <stdio.h>
#include <string.h>

int main(void) {
  char frase[] = "Eu amo programar!";
  int length = strlen(frase);

  printf("O comprimento da string é %d\n", length);
  return 0;
}

```
**Saída:**
```
O comprimento da string é 18
```

## Mergulho profundo:

A função ```strlen()``` foi introduzida no padrão C89 e tem sido parte da biblioteca padrão desde então. No entanto, ela não é a única maneira de encontrar o comprimento de uma string em C. Outra opção é usar um loop para percorrer a string e contar o número de caracteres até encontrar o caractere nulo `'\0'`.

Além disso, é importante lembrar que, ao usar a função ```strlen()```, é necessário garantir que a string tenha o caractere nulo no final, caso contrário, o resultado pode ser impreciso. Portanto, é uma boa prática sempre adicionar esse caractere no final das strings.

## Veja também:

- Documentação da função ```strlen()``` na [página oficial do C](https://devdocs.io/c/string/byte/strlen).
- Outras maneiras de encontrar o comprimento de uma string em C [neste artigo](https://www.geeksforgeeks.org/strlen-function-in-c/).