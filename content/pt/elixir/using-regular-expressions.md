---
title:                "Usando expressões regulares"
html_title:           "Gleam: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

As expressões regulares (regex) são uma técnica poderosa para o reconhecimento e manipulação de padrões de texto. Programadores recorrem a elas por sua eficiência em tarefas como validação de dados, busca e substituição de texto.

## Como fazer:
Expressões regulares no Elixir podem ser usadas com o módulo Regex. Aqui está um exemplo de uso:

```elixir
iex> Regex.match?(~r/elixir/, "Hello, Elixir")
false
iex> Regex.match?(~r/elixir/i, "Hello, Elixir")
true
```
No primeiro exemplo, ele retorna `false` porque a correspondência é sensível a maiúsculas e minúsculas, porém no segundo caso adicionamos o `i` no fim do regex para torná-lo insensível ao caso.

## Mergulho Profundo:

As expressões regulares têm uma longa história, sendo derivadas da teoria das linguagens de programação formal. No Elixir, elas são implementadas usando a biblioteca PCRE de alto desempenho, que oferece muitos recursos avançados.

Em termos de alternativas, Elixir oferece outras formas de manipulação de string, como a função `String.split/1` para divisão de strings, e as funções de mapeamento e redução para processamento adicional.

Porém, pense duas vezes antes de substituir as expressões regulares. Eles são uma ferramenta poderosa, mas com grande poder vem grande responsabilidade. A complexidade das expressões regulares pode tornar o seu código difícil de ler e manter.

## Ver também:

A documentação oficial do módulo Regex do Elixir é um excelente local para começar: [Regex - Elixir](https://hexdocs.pm/elixir/Regex.html)

Se você quiser ir além, o capítulo sobre expressões regulares na Learn You Some Erlang é uma leitura excelente: [LYSE: Regular Expressions](http://learnyousomeerlang.com/regular-expressions) 

Para praticar suas habilidades com expressões regulares, confira o exercício no Exercism: [Exercism Elixir Track](https://exercism.io/tracks/elixir)