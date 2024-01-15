---
title:                "Capitulando uma string"
html_title:           "Elixir: Capitulando uma string"
simple_title:         "Capitulando uma string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que

Capitalizar uma string é uma tarefa comum na programação. Isso é especialmente útil quando se deseja padronizar a formatação de uma string para facilitar a leitura e comparação.

## Como fazer

Uma maneira simples de capitalizar uma string em Elixir é utilizando a função `String.capitalize/1`.

```elixir
"olá, mundo" 
|> String.capitalize()
```

Isso produzirá a saída `"Olá, mundo"`, com a primeira letra em maiúscula. Se você precisar capitalizar cada palavra em uma string, pode utilizar a função `String.capitalize_words/1`.

```elixir
"aprendendo elixir" 
|> String.capitalize_words()
```

A saída será `"Aprendendo Elixir"`, com cada palavra começando por uma letra maiúscula. Se precisar capitalizar apenas a primeira letra de cada palavra em uma string, você pode usar a função `String.capitalize_each/1`.

```elixir
"fazendo código em elixir" 
|> String.capitalize_each()
```

A saída será `"Fazendo Código Em Elixir"`, com a primeira letra de cada palavra em maiúscula. Além dessas funções, existem várias outras maneiras de capitalizar uma string em Elixir. Experimente e descubra qual funciona melhor para o seu caso de uso.

## Mergulho profundo

Embora possa parecer uma tarefa simples, a capitalização de strings pode envolver alguns detalhes mais complexos, dependendo do idioma e do contexto. Por exemplo, em alguns idiomas, existem regras específicas para capitalização de nomes próprios e títulos. Portanto, é importante estar ciente dessas nuances ao trabalhar com a capitalização de strings.

Além disso, em Elixir, as strings são imutáveis, o que significa que uma nova string será criada sempre que uma função de capitalização for chamada. Isso pode ser um problema de desempenho em determinados casos, portanto, é importante monitorar seu código e considerar estratégias de otimização se necessário.

## Veja também

- [Documentação oficial do Elixir sobre strings](https://hexdocs.pm/elixir/String.html)
- [Post do blog "The Rubyist's Guide to Elixir - String Manipulation"](https://lobotuerto.com/blog/the-rubyists-guide-to-elixir-string-manipulation)