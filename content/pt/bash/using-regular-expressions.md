---
title:                "Utilizando expressões regulares"
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O que & por quê?

Usar expressões regulares é uma maneira de pesquisar, encontrar e manipular padrões de texto em arquivos e diretórios. Os programadores geralmente usam expressões regulares para automatizar tarefas relacionadas ao processamento de texto, como análise de dados, validação de entrada e formatação de saída.

## Como fazer:

Veja abaixo dois exemplos de expressões regulares em ação no Bash.

1. Encontre todos os arquivos que terminam com ".txt" no diretório atual:
```
Bash ls *.txt
```
2. Substitua todas as ocorrências de "banana" por "maçã" em um arquivo de texto:
```
Bash sed -i 's/banana/maçã/g' arquivo.txt
```

## Imersão profunda:

As expressões regulares têm uma história longa e rica, tendo sido inventadas na década de 1950 pelo matemático Stephen Cole Kleene. Há várias opções de ferramentas para usar expressões regulares, como o comando "grep" e as linguagens de programação Perl e Python. No Bash, as expressões regulares são implementadas por meio de metacaracteres e operadores especiais, que permitem uma grande variedade de funcionalidades.

## Veja também:

- [O que são expressões regulares?](https://techterms.com/definition/regex)
- [Guia de expressões regulares do Bash](https://www.gnu.org/software/sed/manual/html_node/Regular-Expressions-in-sed.html)
- [Introdução às expressões regulares em Perl](https://www.perl.com/pub/2004/06/18/regexes.html/)