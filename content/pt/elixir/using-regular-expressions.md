---
title:                "Utilizando expressões regulares"
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que é & Por Que Usar?
Expressões regulares são padrões que definem conjuntos de strings. Programadores as utilizam para buscar, substituir e validar textos de maneira rápida e flexível.

## Como Fazer:
```elixir
# Encontrando padrões
regex = ~r/hello/
"Hola! Hello! Hallo!" |> String.split() |> Enum.filter(&Regex.match?(regex, &1))
# Saída: ["Hello!"]

# Substituindo texto
String.replace("2023 Ano do Café", ~r/\d+/, "2022")
# Saída: "2022 Ano do Café"

# Validando um email
Regex.match?(~r/^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$/, "exemplo@email.com")
# Saída: true
```

## Aprofundamento
Expressões regulares, ou regex, surgiram na década de 1950, derivadas de teorias formais da computação e linguística. Alternativas a regex incluem o parseamento de strings com algoritmos específicos ou o uso de bibliotecas de análise sintática. Em Elixir, regex é implementado por meio da biblioteca Erlang `:re`, que por sua vez é baseada na biblioteca PCRE - Perl Compatible Regular Expressions.

## Veja Também
- [Documentação oficial de Regex em Elixir](https://hexdocs.pm/elixir/Regex.html)