---
title:                "Escrevendo para o erro padrão"
html_title:           "Elixir: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O que e por que?

Escrever para o erro padrão é uma técnica comum usada por programadores para direcionar mensagens de erro e depuração em seus programas. Ao invés de imprimir essas mensagens na saída padrão, elas são enviadas para o erro padrão, que é normalmente redirecionado para o console ou registrado em um arquivo de log. Isso permite que os programadores vejam essas mensagens de forma separada da saída do programa, facilitando a identificação e correção de erros.

## Como fazer:

```elixir
IO.puts("Olá, mundo!") # Saída padrão
IO.puts("Algo deu errado.") # Saída de erro

# Saída:
Olá, mundo!
Algo deu errado.
```

## Mergulho profundo:

Escrever para o erro padrão é uma prática comum que remonta aos primórdios da programação. Antes dos consoles e arquivos de log, os programadores costumavam exibir mensagens de erro diretamente no terminal, mesmo que não fossem lançadas como exceções. Existem diversas formas de se escrever para o erro padrão em Elixir, sendo a mais comum através do uso da função `IO.puts/1`. Outra opção é utilizar o módulo `Logger` para registrar mensagens de erro de forma mais estruturada em arquivos de log.

## Veja também:

- Documentação do Elixir: http://elixir-lang.org/docs.html
- Comunidade brasileira de Elixir: https://elixir-brasil.org/
- Blog oficial do Elixir: https://elixir-lang.org/blog/