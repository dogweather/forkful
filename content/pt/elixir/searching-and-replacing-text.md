---
title:                "Elixir: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que usar busca e substituição de texto em Elixir?

Substituição de texto é uma tarefa comum em programação, especialmente quando se trabalha com dados de texto estruturados. Em Elixir, existem várias maneiras de realizar essa tarefa, tornando-a uma linguagem versátil e poderosa para manipulação de strings.

## Como fazer busca e substituição de texto em Elixir

Para fazer uma busca simples e substituir todas as ocorrências de uma string em outra, podemos usar a função `String.replace/4`. Por exemplo:

```Elixir
iex> String.replace("Olá mundo", "Olá", "Oi")
"Oi mundo"
```

Também podemos usar expressões regulares para procurar padrões específicos em uma string e substituí-los por outro texto. Isso é feito usando a função `Regex.replace/3`. Por exemplo:

```Elixir
iex> Regex.replace("ABC123", ~r/[A-Z]/, "X")
"XBC123"
```

Podemos até mesmo usar uma função de callback para fornecer lógica personalizada para a substituição. Vamos dar uma olhada em um exemplo:

```Elixir
iex> String.replace("Gosto de comer maçãs e bananas", "ã", fn _ -> "ê" end)
"Gosto de comer maçês e bananas"
```

## Mais informações sobre busca e substituição de texto em Elixir

Além das funções mencionadas acima, Elixir também oferece outras opções para busca e substituição de texto, como `String.replace_first/4` para substituir apenas a primeira ocorrência de uma string, e `String.replace/3` para ignorar maiúsculas e minúsculas durante a substituição.

Podemos também usar a biblioteca `nimble_parsec` para fazer buscas e substituições baseadas em gramáticas.

## Veja também

- [Documentação oficial do Elixir para String](https://hexdocs.pm/elixir/String.html)
- [Guia essencial de expressões regulares em Elixir](https://flaviocopes.com/elixir-regex/)
- [Elixir School: Aprenda Elixir de forma divertida e rápida](https://elixirschool.com/pt/)