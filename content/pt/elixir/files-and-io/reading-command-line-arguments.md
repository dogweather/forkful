---
date: 2024-01-20 17:55:52.862037-07:00
description: "Ler argumentos da linha de comando permite que programas em Elixir recebam\
  \ dados externos quando s\xE3o iniciados, tornando-os customiz\xE1veis e flex\xED\
  veis.\u2026"
lastmod: 2024-02-19 22:05:05.330047
model: gpt-4-1106-preview
summary: "Ler argumentos da linha de comando permite que programas em Elixir recebam\
  \ dados externos quando s\xE3o iniciados, tornando-os customiz\xE1veis e flex\xED\
  veis.\u2026"
title: Lendo argumentos da linha de comando
---

{{< edit_this_page >}}

## What & Why?
Ler argumentos da linha de comando permite que programas em Elixir recebam dados externos quando são iniciados, tornando-os customizáveis e flexíveis. Programadores usam esses argumentos para modificar o comportamento do programa sem mudar o código.

## How to:
Comece com um projeto Elixir. Aqui está o que você precisa:

```elixir
# Para capturar argumentos da linha de comando, use System.argv():

defmodule CommandLineExample do
  def main(args \\ System.argv()) do
    IO.inspect(args)
  end
end

# Para rodar, chame `CommandLineExample.main` na shell interativa:
CommandLineExample.main(["arg1", "arg2", "arg3"])
# Saída esperada: ["arg1", "arg2", "arg3"]
```
Para executar com argumentos de linha de comando reais, compile e execute o seguinte:

```elixir
# compile o código
elixirc command_line_example.exs

# execute com argumentos
elixir -e "CommandLineExample.main" -- arg1 arg2 arg3
# Saída esperada: ["arg1", "arg2", "arg3"]
```

## Deep Dive
Historicamente, Elixir, assim como muitas linguagens no universo de Erlang VM, é usada para sistemas distribuídos. Argumentos da linha de comando são essenciais para iniciar nodos, configurar detalhes do sistema e passar flags de ambiente.

Alternativamente, se você precisa de análise de argumentos mais sofisticada, como flags, olhe para bibliotecas externas como `OptionParser`:

```elixir
parsed_args = OptionParser.parse(args, switches: [mode: :string])
```

Ao ler argumentos da linha de comando diretamente, o Elixir os trata como uma lista de strings. Todo o parsing e a validação são por sua conta, o que é ótimo para controle granular, mas pode ser trabalhoso para casos mais complexos.

## See Also
- [Getting Started Guide](https://elixir-lang.org/getting-started/introduction.html)
- [Erlang's documentation on argument handling](http://erlang.org/doc/man/init.html#get_arguments-0)
