---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concatenação de Strings em Elixir: Um Guia Rápido

## O Que é & Por Quê?

Concatenação de strings é simplesmente a técnica de combinar duas ou mais strings em uma única string. É crucial para os programadores porque aumenta a legibilidade e eficiência do código, permitindo a manipulação e exibição dinâmica de textos.

## Como Fazer:

Em Elixir, você pode usar `"<>"` para concatenar strings. Veja como é feito:

```elixir
string1 = "Olá, "
string2 = "mundo!"
saida = string1 <> string2
IO.puts(saida)
```
A saída será:

```elixir
"Olá, mundo!"
```
## Mergulho Profundo

Historicamente, Elixir fornece suporte embutido para manipulação de strings devido ao seu fundamento na máquina virtual Erlang. Antes da adoção de "<>", outros métodos eram usados, mas eles eram menos intuitivos e mais verbosos. Alternativas para a concatenação de strings incluem o uso de listas de caracteres e a interpolação de strings. No entanto, "<>" é mais recomendado por ser o mais claro e eficiente. Implementa-se a concatenação de strings em Elixir ao nível de byte, o que a torna extremamente rápida e eficiente.

## Veja Também

Para mais informações sobre a manipulação de strings em Elixir, você pode conferir as seguintes fontes:

- Documentação Oficial de Strings de Elixir: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- [https://elixir-lang.org/getting-started/basic-types.html#strings](https://elixir-lang.org/getting-started/basic-types.html#strings)
- Postagem do Blog Elixir School: [https://elixirschool.com/en/lessons/basics/strings/](https://elixirschool.com/en/lessons/basics/strings/)