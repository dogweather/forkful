---
title:                "Escrevendo para o erro-padrão"
html_title:           "Elixir: Escrevendo para o erro-padrão"
simple_title:         "Escrevendo para o erro-padrão"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que

Escrever para a saída padrão de erro (standard error) é uma forma de melhorar a usabilidade e confiabilidade de seus programas Elixir. Além disso, isso pode ajudar a solucionar problemas e depurar erros em seu código de forma mais eficiente.

## Como Fazer

Usar a função `IO.puts/2` com o `:stderr` como primeiro argumento é a maneira mais simples de escrever para a saída de erro padrão em Elixir. Observe o código abaixo como exemplo:

```Elixir
IO.puts(:stderr, "Este é um erro de exemplo.")
```

Isso irá imprimir a mensagem "Este é um erro de exemplo." na saída de erro padrão. Você também pode usar outras funções de saída, como `IO.inspect/2`, para direcionar a saída para a saída de erro padrão.

## Mergulho Profundo

Além de simplesmente imprimir mensagens de erro, também é possível formatar e colorir a saída para facilitar a identificação dos erros. Você pode usar a biblioteca `ANSI` para adicionar cores aos seus erros, veja o exemplo abaixo:

```Elixir
require ANSI

IO.puts(:stderr, "Este é um " <> ANSI.colorize("erro", :red) <> " de exemplo.")
```

Isso irá imprimir a mensagem "Este é um erro de exemplo." em vermelho na saída de erro padrão. Além disso, você também pode definir os códigos de saída para diferentes tipos de erros, o que pode ajudar na identificação e tratamento desses erros.

## Veja Também

- Documentação oficial Elixir sobre escrita para a saída de erro padrão: https://hexdocs.pm/elixir/IO.html#puts/2
- Tutorial sobre saída de erro padrão em Elixir: https://elixircasts.io/error-handling-in-elixir-using-puts-to-print-to-stderr