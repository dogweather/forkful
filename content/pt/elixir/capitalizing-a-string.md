---
title:                "Elixir: Transformando uma string em letra maiúscula"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que
A capitalização de strings é uma habilidade básica que muitos programadores precisam dominar para lidar com dados do mundo real. Aprender a capitalizar strings em Elixir pode ajudar a tornar seu código mais eficiente e legível.

## Como Fazer
Aqui está um exemplo simples de como capitalizar uma string em Elixir:

```Elixir
iex> String.capitalize("elixir")
"Elixir"
```

Se você quiser capitalizar apenas a primeira letra de uma string, pode usar a função `capitalize_first`:

```Elixir
iex> String.capitalize_first("elixir")
"Elixir"
```

E se você tiver uma string com várias palavras, pode usar a função `capitalize_words` para capitalizar a primeira letra de cada palavra:

```Elixir
iex> String.capitalize_words("elixir programming")
"Elixir Programming"
```

## Mergulho Profundo
Por baixo dos panos, a função `capitalize` usa a função `capitalize_at` para capitalizar apenas a primeira letra da string. Você também pode usar `capitalize_at` diretamente, especificando o índice da letra que deseja capitalizar:

```Elixir
iex> String.capitalize_at("elixir", 0)
"Elixir"
```

Outra função útil para capitalizar strings em Elixir é a `titlecase`, que capitaliza a primeira letra de cada palavra e coloca o resto em minúsculas:

```Elixir
iex> String.titlecase("elixir IS awesome")
"Elixir Is Awesome"
```

## Veja Também
- [Documentação oficial do módulo String](https://hexdocs.pm/elixir/String.html)
- [Tutorial Interativo sobre Strings em Elixir](https://elixir-lang.org/getting-started/string-sigils-and-interpolation.html#strings)
- [Artigo sobre as cinco funções básicas de manipulação de strings em Elixir](https://sergiotapia.com/5-string-manipulation-functions-you-should-know-in-elixir/)

Espero que este artigo tenha ajudado você a entender melhor como capitalizar strings em Elixir. Continue praticando e explorando os recursos do módulo String para se tornar um mestre em manipulação de strings!