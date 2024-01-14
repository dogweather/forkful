---
title:                "Gleam: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Você já se deparou com um texto longo que precisava ser alterado? Ou talvez você esteja trabalhando em um projeto com vários arquivos e precisa mudar uma palavra em todos eles? A busca e substituição de texto pode ser uma tarefa tediosa e demorada, mas com o Gleam, você pode simplificá-la!

## Como fazer

Para realizar uma busca e substituição de texto com o Gleam, siga os seguintes passos:

1. Importe o módulo `gleam/string`.
2. Utilize a função `replace` para especificar o texto que deseja substituir e qual será o novo texto.
3. Utilize a função `replace_all` para realizar a substituição em todas as ocorrências do texto especificado.

```
Gleam import gleam/string

let texto = "Gleam é uma linguagem de programação moderna."
let novo_texto = replace(texto, "Gleam", "Elixir")
let texto_final = replace_all(novo_texto, "linguagem", "plataforma")

```

A saída do código acima será: `Elixir é uma plataforma de programação moderna.`

Note que a função `replace_all` substitui todas as ocorrências do texto especificado, enquanto a função `replace` substitui apenas a primeira ocorrência.

## Deep Dive

Há muitos outros recursos na biblioteca `gleam/string` que podem ser utilizados para realizar uma busca e substituição de texto eficiente. Algumas delas incluem:

- Funções para ignorar letras maiúsculas/minúsculas: `replace_ignore_case` e `replace_all_ignore_case`.
- Função para substituir texto em uma posição específica: `replace_at`.
- Função para substituir texto com base em um padrão: `replace_regex`.

Familiarizar-se com essas funções pode facilitar ainda mais a sua vida ao realizar uma busca e substituição de texto com o Gleam.

## Veja também

- Documentação oficial do Gleam sobre a biblioteca `gleam/string`: https://gleam.run/packages/gleam/string
- Tutorial sobre como utilizar a biblioteca `gleam/string`: https://bloggleam.com/replace-text-with-gleam-string-library/