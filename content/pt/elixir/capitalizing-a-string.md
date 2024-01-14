---
title:    "Elixir: Convertendo uma string em maiúsculas"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string?

Capitalizar uma string pode ser útil em várias situações, como por exemplo, quando estamos trabalhando com nomes de pessoas. Em vez de ter que gerenciar diferentes formas de capitalização, podemos simplesmente utilizar uma função que capitalize a string inteira.

## Como capitalizar uma string?

Para capitalizar uma string em Elixir, podemos utilizar a função `String.capitalize/1`. Veja o código de exemplo abaixo:

```elixir
iex> string = "elixir é incrível"
iex> String.capitalize(string)
"Elixir é incrível"
```

Podemos também utilizar a função `String.capitalize/2` para especificar um idioma diferente para a capitalização. Por exemplo:

```elixir
iex> string = "fast and furious"
iex> String.capitalize(string, :es)
"Fast And Furious"
```

## Mergulho profundo

A função `String.capitalize/1` utiliza a biblioteca `Unicode` por trás das cenas para realizar a capitalização. Isso significa que ele irá funcionar corretamente com caracteres UTF-8, incluindo caracteres acentuados e símbolos. Além disso, a função também irá respeitar as regras gramaticais do idioma especificado.

## Veja também

- [Documentação oficial da função `String.capitalize/1` em Elixir](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [Documentação oficial da biblioteca `Unicode` em Elixir](https://hexdocs.pm/elixir/Unicode.html)