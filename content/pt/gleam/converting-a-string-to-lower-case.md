---
title:                "Convertendo uma string para minúsculas"
html_title:           "Gleam: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O Que e Por Que?

A conversão de uma string para caixa baixa é o processo de alterar todas as letras maiúsculas em uma string para letras minúsculas. Isso é feito por programadores para facilitar a comparação de strings, pois as letras maiúsculas e minúsculas são tratadas de forma diferente pelos computadores.

## Como Fazer:

Para converter uma string para caixa baixa em Gleam, use a função `String.to_lower`. Por exemplo:

```
Gleam

let string = "Olá, Amigos!"
let lower_case_string = String.to_lower(string)

```
Saída:
```
"olá, amigos!"
```

## Mergulho Profundo:

A conversão de strings para caixa baixa é uma prática comum em programação, pois oferece uma maneira mais fácil de comparar strings que podem variar em letras maiúsculas e minúsculas. Existem muitos outros métodos para realizar essa tarefa, como usar expressões regulares ou funções de loop. No entanto, a função `String.to_lower` em Gleam é uma solução simples e eficaz.

## Veja Também:

Para saber mais sobre o uso da função `String.to_lower` em Gleam, confira a documentação oficial em https://gleam.run/types/string.html#to-lower. Você também pode aprender mais sobre strings e suas operações em https://gleam.run/types/string.html.