---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Elixir: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O que e Porque?

Encontre e delete caracteres que correspondam a um determinado padrão é um processo comum para programadores Elixir. Isso ajuda a limpar e formatar dados de forma mais eficiente e consistente. Além disso, isso também pode ser usado para filtrar e selecionar dados específicos em uma coleção.

## Como Fazer:

```Elixir
"Hello, world!" |> String.replace("l", "")
```
`"Heo, word!"`

```Elixir
"12345" |>String.replace(~r/\d/, "")
```
`""`

Para excluir caracteres correspondentes de uma string, basta usar a função `String.replace/3` e passar o padrão a ser correspondido como o segundo argumento. Se o padrão for uma expressão regular, use `~r//` para envolvê-lo. O resultado será a string original com os caracteres correspondentes removidos.

## Mergulho Profundo:

A exclusão de caracteres correspondentes é um recurso comumente usado em linguagens de programação e Elixir não é exceção. No entanto, vale ressaltar que Elixir é uma linguagem funcional e, portanto, encoraja o uso de funções puras. Isso significa que, em vez de modificar a string original, a função `String.replace/3` retorna uma nova string com os caracteres correspondentes removidos.

Além disso, em vez de usar `String.replace/3`, também é possível usar funções como `String.delete_prefix/2` e `String.delete_suffix/2` para remover segmentos específicos de uma string.

## Veja Também:

Documentação oficial do Elixir sobre `String.replace/3`: https://hexdocs.pm/elixir/String.html#replace/3

Outros métodos de manipulação de strings em Elixir: https://dev.to/5t3ph/gentle-introduction-to-manipulating-strings-in-elixir-heo