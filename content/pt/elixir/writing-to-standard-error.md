---
title:                "Escrevendo no erro padrão"
date:                  2024-01-19
simple_title:         "Escrevendo no erro padrão"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O Que É & Por Que Usar?
Escrever no erro padrão (stderr) é uma forma de enviar mensagens de erro e diagnósticos que não fazem parte da saída principal de um programa. Programadores fazem isso para separar a saída normal de mensagens de erro, facilitando a depuração e o log de erros.

## Como Fazer:
```elixir
# Enviar uma mensagem para o stderr
:io.format(:standard_error, "Erro: algo deu errado!~n", [])

# Verificar no terminal a mensagem enviada para o stderr
# Saída esperada: "Erro: algo deu errado!"
```

## Mergulho Profundo
Historicamente, o conceito de separar streams de saída normal e erro vem do Unix. Elixir, rodando na Erlang VM, adere a essa convenção. Alternativamente, poderia-se usar `IO.warn/1` para warnings, que também usa stderr. Do ponto de vista da implementação, a Erlang VM gerencia a saída para stderr por baixo dos panos, que pode ser redirecionada ou manipulada por sistemas operacionais conforme necessário.

## Veja Também
- [IO — Elixir](https://hexdocs.pm/elixir/IO.html)
- [Erlang :io module](http://erlang.org/doc/man/io.html)

Note que links são para documentações em inglês, já que podem não estar disponíveis em português.
