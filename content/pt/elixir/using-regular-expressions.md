---
title:                "Utilizando expressões regulares"
html_title:           "Elixir: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O que & por quê?
Expressões regulares são padrões de caracteres usados para encontrar e manipular texto em dados. Programadores usam expressões regulares para buscar, substituir ou validar padrões em strings de texto.

## Como usar:
```
Elixir
import Regex
Regex.match?(~r/[aeiou]/, "Hello World") # => true
Regex.replace(~r/hello/, "Hello World", "Hi") # => "Hi World"
```

## Mergulho Profundo:
As expressões regulares foram desenvolvidas pelo matemático americano Stephen Kleene na década de 1950. Existem alternativas para expressões regulares, como o pacote `String` do Elixir e o `grep` do Unix, mas eles oferecem funcionalidade limitada em comparação. Internamente, o Elixir usa a biblioteca `PCRE` para implementar as expressões regulares.

## Veja também:
[Documentação oficial do Elixir sobre expressões regulares](https://hexdocs.pm/elixir/Regex.html)