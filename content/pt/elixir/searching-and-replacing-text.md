---
title:                "Buscando e substituindo texto"
html_title:           "Elixir: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Substituir texto é uma tarefa comum na programação, que consiste em encontrar determinado texto em um arquivo ou string e substituí-lo por outro texto. Os programadores geralmente fazem isso para corrigir erros, atualizar informações ou realizar transformações nos dados.

## Como fazer:

```
Elixir
str = "Hello World!"
new_str = String.replace(str, "Hello", "Hi")

IO.puts new_str  # Output: Hi World!
```

No exemplo acima, utilizamos a função `String.replace` para substituir a palavra "Hello" por "Hi" na string "Hello World!". Em Elixir, essa função recebe três argumentos: a string original, o texto a ser substituído e o novo texto.

Você também pode usar expressões regulares para substituir texto com mais precisão. Por exemplo:

```
Elixir
str = "abc123def456"
new_str = String.replace(str, ~r/[a-z]+(\d+)/, "\\1")

IO.puts new_str  # Output: 123456
```

No exemplo acima, utilizamos uma expressão regular para encontrar e substituir todos os caracteres alfabéticos seguidos de números por apenas os números.

## Mergulho Profundo:

A substituição de texto tem sido uma tarefa fundamental na programação desde os primórdios da linguagem de programação. Com o surgimento de expressões regulares, essa tarefa se tornou ainda mais poderosa e flexível.

Além da função `String.replace`, Elixir também oferece outras opções para substituição de texto, como `String.replace_at`, que substitui o texto em uma posição específica, e `String.replace_prefix`, que substitui o texto no início de uma string. Também existem diversas bibliotecas de terceiros que oferecem funcionalidades adicionais para substituição de texto.

## Veja também:

- [Documentação do Elixir sobre substituição de texto](https://hexdocs.pm/elixir/1.12/String.html#replace/3)
- [Guia de expressões regulares em Elixir](https://www.thegreatcodeadventure.com/regex-in-elixir/)
- [Comparação entre as diferentes funções de substituição de texto em Elixir](https://blog.appsignal.com/2021/05/05/elixirs-string-replace-functions.html)