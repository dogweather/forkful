---
title:                "Convertendo uma string para letras minúsculas"
aliases:
- /pt/c/converting-a-string-to-lower-case.md
date:                  2024-02-03T17:54:33.361642-07:00
model:                 gpt-4-0125-preview
simple_title:         "Convertendo uma string para letras minúsculas"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/converting-a-string-to-lower-case.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

Converter uma string para minúsculas em C envolve transformar todas as letras maiúsculas de uma string dada em suas correspondentes minúsculas. Programadores frequentemente realizam essa operação para padronizar a entrada de texto para comparação, operações de busca, ou simplesmente para consistência estética na saída.

## Como fazer:

C não possui uma função embutida para conversão de string para minúsculas diretamente, ao contrário de algumas linguagens de alto nível. No entanto, o processo pode ser facilmente implementado usando as funções da biblioteca padrão do C. Abaixo está um guia passo a passo e um exemplo ilustrando como converter uma string para minúsculas.

```c
#include <stdio.h>
#include <ctype.h>

void toLowerCase(char *str) {
    while (*str) {
        *str = tolower(*str);
        str++;
    }
}

int main() {
    char texto[] = "Hello, World!";
    printf("Original: %s\n", texto);

    toLowerCase(texto);
    printf("Minúsculas: %s\n", texto);

    return 0;
}
```

**Saída de Amostra:**

```
Original: Hello, World!
Minúsculas: hello, world!
```

Neste exemplo, a função `toLowerCase` itera através de cada caractere da string de entrada, convertendo-o para sua equivalente em minúscula usando a função `tolower` de `ctype.h`. A modificação é feita no local, alterando a string original.

## Aprofundando

A função `tolower` usada no exemplo acima faz parte da biblioteca padrão do C, especificamente dentro do arquivo cabeçalho `ctype.h`. Ela opera baseada no local atual, mas para o local "C" padrão, ela lida com o conjunto de caracteres ASCII, onde 'A' até 'Z' são convertidos em 'a' até 'z'.

Historicamente, o tratamento de codificação de caracteres e conversão de caixa em C estava fortemente ligado ao conjunto de caracteres ASCII, limitando sua utilidade em aplicações internacionais ou localizadas onde caracteres fora do conjunto ASCII são comuns. Linguagens de programação modernas podem oferecer métodos de string embutidos para realizar a conversão de caixa considerando localidade e caracteres Unicode, o que o C não possui nativamente.

Em cenários que requerem manipulação extensiva de texto, especialmente com caracteres não-ASCII, programadores podem considerar o uso de bibliotecas que oferecem melhor suporte à internacionalização, como o ICU (International Components for Unicode). No entanto, para a maioria das aplicações que lidam com texto ASCII, a abordagem demonstrada é eficiente e direta. Isso destaca a propensão do C em dar aos programadores controle sobre a manipulação de dados, embora com um pouco mais de esforço envolvido em comparação com linguagens de nível mais alto.
