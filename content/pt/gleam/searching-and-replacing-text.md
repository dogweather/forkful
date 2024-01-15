---
title:                "Busca e substituição de texto"
html_title:           "Gleam: Busca e substituição de texto"
simple_title:         "Busca e substituição de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que fazer busca e substituição de texto?

Fazer busca e substituição de texto é uma tarefa comum na programação. Imagine que você tenha um código com muitas linhas repetitivas e queira substituir uma parte específica em todas elas. Em vez de fazer a alteração manualmente em cada linha, você pode usar a funcionalidade de busca e substituição para automatizar o processo. Isso economiza tempo e reduz o risco de erros no código.

## Como fazer busca e substituição em Gleam

Em Gleam, a funcionalidade de busca e substituição é feita com a função `String.replace()`. Esta função recebe dois argumentos: o texto original e o texto a ser substituído.

```
Gleam> String.replace("Hello World!", "World", "Universe")
"Hello Universe!"
```
Neste exemplo, a função substituiu a palavra "World" pela palavra "Universe" na string "Hello World!".

Se você quiser substituir todas as ocorrências de uma palavra em uma string, pode adicionar o modificador "g" ao final do texto original.

```
Gleam> String.replace("Hello Hello Hello", "Hello", "Hey", "g")
"Hey Hey Hey"
```

## Aprofundando na busca e substituição de texto

Além de substituir textos simples, você também pode usar expressões regulares na função `String.replace()`. Expressões regulares são padrões de texto que permitem fazer substituições mais complexas.

Por exemplo, digamos que você queira substituir todas as ocorrências de números em uma string por asteriscos. Você pode usar a expressão regular `[0-9]` para identificar os números e substituí-los por "*".

```
Gleam> String.replace("123abc456def", "[0-9]", "*", "g")
"***abc***def"
```

Existem muitos outros padrões e combinações que podem ser usados com expressões regulares para realizar substituições precisas e avançadas.

## Veja também

- Documentação oficial Gleam sobre `String.replace()`: https://gleam.run/modules/std.String.html#type.string:0x1e720b9771f1a033
- Tutorial sobre expressões regulares: https://www.regular-expressions.info/tutorial.html