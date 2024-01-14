---
title:                "C: Convertendo uma string para minúsculas"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por Que

Quando se trabalha com programação, é comum lidar com strings, que são conjuntos de caracteres. Em algumas situações, pode ser necessário alterar o formato de uma string, como por exemplo, transformá-la em letras minúsculas. Neste artigo, vamos explorar como fazer essa conversão em C.

## Como Fazer

Para converter uma string para letras minúsculas em C, podemos utilizar a biblioteca string.h e a função strlwr(). Esta função recebe como parâmetro a string que será convertida e retorna a mesma string com todas as letras em minúsculo.

```
#include <stdio.h>
#include <string.h>

int main()
{
    char string[20] = "Olá MUNDO";
    printf("String original: %s\n", string);
    printf("String em minúsculo: %s\n", strlwr(string));

    return 0;
}

```

Neste exemplo, a string original é "Olá MUNDO" e a função strlwr() retorna "olá mundo". Podemos ver que todas as letras foram convertidas para minúsculo.

## Mergulho Profundo

Por trás da função strlwr(), existe um processo de conversão que ocorre. Primeiramente, é importante saber que em C, as letras maiúsculas e minúsculas são representadas por valores diferentes na tabela ASCII. As minúsculas possuem valores mais altos do que as maiúsculas. Dessa forma, quando utilizamos a função strlwr(), ela percorre a string e altera o valor de cada letra para um valor mais alto, resultando em letras minúsculas.

No entanto, é importante lembrar que a função strlwr() não funciona com todos os idiomas. Ela funciona apenas com caracteres da língua inglesa. Para lidar com outros idiomas, é necessário utilizar outras funções e técnicas de programação.

## Veja Também

- [Documentação do strlwr() em C](https://www.tutorialspoint.com/c_standard_library/c_function_strlwr.htm)
- [ASCII Table](https://www.asciitable.com/)
- [Outras funções para lidar com strings em C](https://www.programiz.com/c-programming/c-strings)