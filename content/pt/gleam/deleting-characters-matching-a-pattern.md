---
title:                "Excluindo caracteres que correspondem a um padrão"
date:                  2024-01-20T17:42:18.683509-07:00
model:                 gpt-4-1106-preview
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (O Quê & Por Quê?)
Deletar caracteres que correspondem a um padrão é sobre filtrar o que não queremos em strings. Programadores fazem isso para limpar dados, validar entradas ou simplesmente formatar informações da forma que precisam.

## How to (Como fazer):
Aqui está como você pode remover caracteres de uma string em Gleam baseado em um padrão, usando regex:

```gleam
import gleam/regex.{Regex, replace}

pub fn main() {
  let pattern = Regex("(\\d)", regex::None).unwrap()
  let result = replace(pattern, "Contato: 1234-5678", "", replace::All)
  io.println(result) // Imprime "Contato: -"
}
```

E a saída seria:

```
"Contato: -"
```

## Deep Dive (Mergulho Profundo):
Historicamente, a manipulação de strings é uma área fundamental da programação, e regex (expressões regulares) têm sido uma ferramenta poderosa desde os anos 1950 para esse propósito. Em Gleam, a biblioteca `gleam/regex` é um recurso que você tem à disposição, mas há alternativas, como trabalhar com funções de alto nível para substituir ou fatiar strings sem regex.

Quanto aos detalhes de implementação, Gleam costuma compilar para Erlang, que por sua vez é conhecido pela sua robusta manipulação de strings e binários. Ao deletar caracteres usando padrões, na verdade você está utilizando parte desse poder sob o capô.

## See Also (Veja Também):
- Regex tutorial: [https://www.regular-expressions.info/tutorial.html](https://www.regular-expressions.info/tutorial.html)
- Erlang's string processing capabilities: [https://erlang.org/doc/apps/stdlib/unicode_usage.html](https://erlang.org/doc/apps/stdlib/unicode_usage.html)
