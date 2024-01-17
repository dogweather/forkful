---
title:                "Excluindo caracteres que correspondem a um padrão."
html_title:           "Gleam: Excluindo caracteres que correspondem a um padrão."
simple_title:         "Excluindo caracteres que correspondem a um padrão."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O que é e por quê?
Quando estamos programando, muitas vezes precisamos deletar caracteres específicos de uma string ou de uma lista. Podemos fazer isso manualmente, mas quando lidamos com grandes quantidades de dados, isso pode ser demorado e propenso a erros. É por isso que existe um recurso chamado "deletar caracteres que correspondem a um padrão", que nos permite automatizar esse processo e economizar tempo e esforço.

## Como fazer:
Para deletar caracteres que correspondem a um padrão em Gleam, usamos a função `String.replace` ou `List.filter_map`. Vamos dar uma olhada em um exemplo simples usando a função `String.replace`:

```Gleam
let original = "Gleam é a melhor linguagem de programação!"
String.replace(original, "melhor", "incrível")
```
Saída: `Gleam é a incrível linguagem de programação!`

Podemos ver que a função substituiu a palavra "melhor" por "incrível" na string original.

## Mergulho profundo:
Este recurso é amplamente utilizado em expressões regulares, que são sequências de caracteres usadas para buscar e manipular padrões em strings. Além disso, existem outras formas de deletar caracteres que correspondem a um padrão, como usando a função `String.trim` para remover espaços em branco no início e no final de uma string. Na implementação de Gleam, essa função é baseada na biblioteca de expressões regulares do Rust.

## Veja também:
Para saber mais sobre a função `String.replace`, você pode conferir a documentação oficial do Gleam: https://gleam.run/documentation/standard-library#string-functions. Além disso, se você quiser se aprofundar em expressões regulares, pode conferir a documentação do Rust: https://doc.rust-lang.org/std/primitive.str.html#method.trim.