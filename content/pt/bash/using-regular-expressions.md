---
title:                "Usando expressões regulares"
html_title:           "Gleam: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Expressões regulares (regex) são um sistema para encontrar e manipular partes específicas de texto. Programadores usam porque facilita a procura, substituição e manipulação de textos complexos e variados.

## Como Fazer:

No Bash, usamos `grep` para usar expressões regulares. Aqui estão alguns exemplos:

```Bash
# Encontre linhas que contêm 'abc'
grep 'abc' arquivo.txt

# Encontre linhas que começam com 'abc'
grep '^abc' arquivo.txt

# Encontre linhas que terminam com 'abc'
grep 'abc$' arquivo.txt
```
Output:
```
abc é fácil
Isso é abc
Eu amo abc
```

## Mergulho Profundo

Expressões regulares foram desenvolvidas na decada de 1950 e estão presentes em muitas linguagens de programação. Alternativas incluem o uso de funções de manipulação de strings inerentes a cada linguagem. No entanto, expressões regulares vêm com o benefício adicional de serem universais e poderosas.

## Veja Também

- Livros e Tutoriais:
1. "Mastering Regular Expressions" por Jeffrey E.F. Friedl
2. [RegexOne](https://regexone.com/): Tutorial interativo online


- Websites Oficiais:
1. [GNU Bash](https://www.gnu.org/software/bash/)
2. [GNU grep](https://www.gnu.org/software/grep/)

- Guias de estilo e boas práticas:
1. Google's Shell Style Guide: [Guide](https://google.github.io/styleguide/shellguide.html)