---
title:                "Pesquisando e substituindo texto"
date:                  2024-01-20T17:58:02.404173-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pesquisando e substituindo texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Pesquisar e substituir texto é o processo de encontrar sequências específicas em uma string e trocá-las por outras. Programadores usam essa técnica para corrigir erros, atualizar dados ou manipular entrada de texto de forma eficiente.

## Como fazer:
```gleam
import gleam/string

pub fn replace_example() {
  let text = "As raposas são astutas e as corujas são sábias."
  let new_text = string.replace(text, "raposas", "gatos")
  new_text
}

// Saída: "Os gatos são astutos e as corujas são sábias."
```

## Mergulho Profundo
Historicamente, a necessidade de buscar e substituir texto vem da edição de texto e programação desde os primeiros dias dos computadores. Ferramentas de linha de comando como `sed` em sistemas Unix são exemplos clássicos de manipulação de texto. Em Gleam, isso é tratado com funções na biblioteca `gleam/string`, que considera a ergonomia e o desempenho. Alternativas incluem expressões regulares, mas o Gleam opta por simplicidade e tipagem forte para evitar erros comuns em outras linguagens.

## Veja Também
- [Elixir String Replace Function](https://hexdocs.pm/elixir/String.html#replace/4), já que Gleam é frequentemente usado em conjunto com o Elixir.
- [Regex Tutorial for Text Manipulation](https://www.regular-expressions.info/), para comparação com a abordagem do Gleam.