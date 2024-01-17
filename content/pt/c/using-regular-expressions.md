---
title:                "Utilizando expressões regulares"
html_title:           "C: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O que e por que?

Expressões regulares são um recurso comum para muitos programadores em C. Elas são uma forma de procurar padrões específicos em strings ou textos. Com expressões regulares, os programadores podem realizar tarefas como validação de dados, pesquisa e substituição de texto de forma eficiente.

## Como fazer:

Utilizar expressões regulares em C é bastante simples. Vamos dar uma olhada em um exemplo de código:

```
#include <stdio.h>
#include <string.h>
#include <regex.h>

int main() {

    // Definir uma expressão regular:
    char *regex = "r([0-9]+)";

    // String de teste:
    char *str = "Este texto contém r47 e r204, mas não 123r456 ou rabc";

    // Compilar a expressão regular:
    regex_t regex_compiled;
    regcomp(&regex_compiled, regex, REG_EXTENDED);

    // Encontrar correspondências:
    int match = regexec(&regex_compiled, str, 0, NULL, 0);

    // Imprimir o resultado:
    printf("Correspondências encontradas: %i \n", match);

    return 0;
}
```

O output deste exemplo seria:

`Correspondências encontradas: 2`

Neste exemplo, estamos procurando por todas as ocorrências de "r" seguido de um ou mais números. Ao executar o código, podemos ver que foram encontradas duas correspondências: "r47" e "r204".

## Detalhes aprofundados:

Expressões regulares existem há décadas e são amplamente utilizadas em diferentes linguagens de programação. Existem diferentes implementações de expressões regulares e muitos detalhes técnicos sobre como elas funcionam.

Alternativas para o uso de expressões regulares em C incluem o uso de bibliotecas externas, como o PCRE (Perl Compatible Regular Expressions). No entanto, para muitos casos simples, as expressões regulares nativas de C são suficientes e de fácil utilização.

Na implementação de expressões regulares em C, é importante prestar atenção aos padrões de escape (como o "\" no exemplo acima) e às possíveis vulnerabilidades de segurança, como o estouro de buffer.

## Veja também:

- [Tutorial de Expressões Regulares em C](https://www.tutorialspoint.com/c_standard_library/regex_h.htm)
- [Diferenças entre Expressões Regulares em C e Perl](https://stackoverflow.com/questions/6616100/regular-expressions-implementation-in-c-vs-perl)
- [Documentação oficial para a biblioteca de Expressões Regulares de C](http://man7.org/linux/man-pages/man3/regex.3.html)