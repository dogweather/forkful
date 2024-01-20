---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Buscar e Substituir Texto em Gleam: Um Guia Conciso

## O Que & Por Quê?

Buscar e substituir texto é o processo de encontrar certas palavras ou cadeias de caracteres dentro do texto e substituí-las por outros. Programadores fazem isso, por exemplo, para atualizar ou corrigir erros.

## Como Fazer:

Abaixo está um exemplo básico de como buscar e substituir texto em Gleam.

```gleam
import gleam/string

let text = "Olá, Mundo!"
let newText = string.replace("Mundo", "Gleam", text)
```

Executando o código acima, o resultado seria `"Olá, Gleam!"`.

## Mergulhando Fundo:

1. Contexto histórico: As funções de busca e substituição ganharam popularidade com os editores de texto, facilitando a edição de grandes blocos de texto. Eles foram posteriormente adotados em linguagens de programação.
2. Alternativas: Em algumas linguagens, como o JavaScript, `String.prototype.replace()` é usado, enquanto no Python, `str.replace()`. Em Gleam, usamos `string.replace()`.
3. Detalhes de implementação: Em Gleam, `string.replace()` é uma função pura que retorna uma nova string, ao invés de modificar a string original.