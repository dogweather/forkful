---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Convertendo uma String para Minúsculas em C

## O Que & Por Quê?

Converter string para minúsculas é o processo de transformar todas as letras maiúsculas de uma string em suas respectivas minúsculas. Programadores geralmente fazem isso para uniformizar dados de texto para processamento ou comparação.

## Como fazer:

Aqui está um exemplo simples de como podemos converter uma string para minúsculas em C.

```C
#include <stdio.h>
#include <ctype.h>

int main()
{
    char str[50] = "Olá Mundo!";
    int i = 0;

    while (str[i])
    {
        putchar(tolower(str[i]));
        i++;
    }

    return 0;
}
```

Ao executar este programa, a saída será:

```
olá mundo!
```

Como você pode ver, todas as letras maiúsculas na string original foram convertidas para minúsculas pelo programa.

## Mergulho Profundo

Historicamente, a conversão de strings para minúsculas tem sido uma prática comum em muitas linguagens de programação, especialmente aquelas que diferenciam maiúsculas de minúsculas como C.

Existem outras maneiras de realizar essa tarefa. Por exemplo, você pode escrever seu próprio código para converter manualmente cada caractere. No entanto, a função `tolower` da biblioteca `ctype.h` facilita esse processo para nós.

Em termos de detalhes de implementação, `tolower` verifica se o caractere é uma letra maiúscula. Se for, ele a converte para a minúscula correspondente. Caso contrário, o caractere original é retornado sem modificações.

## Veja Também

'A Biblioteca Padrão C - ctype.h' -https://www.tutorialspoint.com/c_standard_library/ctype_h.htm
'Função tolower' -http://www.cplusplus.com/reference/cctype/tolower/