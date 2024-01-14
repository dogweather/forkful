---
title:                "Gleam: Extraindo subcadeias"
simple_title:         "Extraindo subcadeias"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

Extrair substrings é uma tarefa essencial em muitos projetos de programação. Seja para manipular strings de texto ou para analisar dados, ter a habilidade de extrair pedaços específicos de uma string é uma ferramenta valiosa para qualquer programador. Felizmente, o Gleam oferece uma maneira simples e eficiente de realizar essa tarefa.

## Como fazer

Para extrair uma substring usando o Gleam, você pode usar a função `String.slice()`. Esta função aceita três parâmetros: a string de origem, o início da substring e o comprimento da substring. Por exemplo:

```
Gleam
import String

my_string = "Olá Gleam programadores!"
substring = String.slice(my_string, 5, 5)

```

Neste exemplo, a variável `substring` conterá a string `Gleam`. Você pode alterar os valores dos parâmetros `start` e `length` para extrair diferentes partes da string de origem.

## Deep Dive

Além da função `String.slice()`, o Gleam também oferece a função `String.slice_by_indexes()` para extrair substrings. Esta função aceita como parâmetros a string de origem e uma lista de índices para o início e o fim da substring. Por exemplo:

```
Gleam
import String

my_string = "Hello world!"
substring = String.slice_by_indexes(my_string, [0, 5])

```

Neste exemplo, a variável `substring` conterá a string `Hello`.

Além disso, o Gleam também possui a capacidade de extrair substrings usando expressões regulares. Isso pode ser feito usando a função `Regex.run()`, que aceita a string de origem e a expressão regular como parâmetros. Para mais informações sobre como usar expressões regulares no Gleam, consulte a documentação oficial.

## Veja também

- Documentação oficial do Gleam: https://gleam.run/
- Tutorial de expressões regulares no Gleam: https://gleam.run/book/tour/regexes.html
- Exemplo de extrair substrings no Gleam: https://github.com/gleam-lang/website-examples/blob/master/string_substring.gleam