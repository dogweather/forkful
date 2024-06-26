---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:33.348225-07:00
description: "Como fazer: Elixir usa o ExUnit como seu framework de teste integrado,\
  \ que \xE9 extremamente poderoso e f\xE1cil de usar. Aqui est\xE1 um exemplo b\xE1\
  sico: 1. Crie\u2026"
lastmod: '2024-03-13T22:44:46.241706-06:00'
model: gpt-4-0125-preview
summary: "Elixir usa o ExUnit como seu framework de teste integrado, que \xE9 extremamente\
  \ poderoso e f\xE1cil de usar."
title: Escrevendo testes
weight: 36
---

## Como fazer:
Elixir usa o ExUnit como seu framework de teste integrado, que é extremamente poderoso e fácil de usar. Aqui está um exemplo básico:

1. Crie um novo arquivo de teste no diretório `test` do seu projeto Elixir. Por exemplo, se você está testando um módulo chamado `MathOperations`, seu arquivo de teste poderia ser `test/math_operations_test.exs`.

```elixir
# test/math_operations_test.exs
defmodule MathOperationsTest do
  use ExUnit.Case

  # Este é um caso de teste simples para verificar a função de adição
  test "a adição de dois números" do
    assert MathOperations.add(1, 2) == 3
  end
end
```

Para executar seus testes, use o comando `mix test` no seu terminal. Se a função `MathOperations.add/2` adicionar corretamente dois números, você verá uma saída similar a:

```
..

Finished in 0.03 seconds
1 test, 0 failures
```

Para testes que envolvem serviços externos ou APIs, você pode querer usar bibliotecas de mock, como o `mox`, para evitar acionar os serviços reais:

1. Adicione `mox` às suas dependências em `mix.exs`:

```elixir
defp deps do
  [
    {:mox, "~> 1.0.0", only: :test},
    # outras depêndencias...
  ]
end
```

2. Defina um módulo de mock no seu auxiliar de teste (`test/test_helper.exs`):

```elixir
Mox.defmock(HTTPClientMock, for: HTTPClientBehaviour)
```

3. Utilize o mock no seu caso de teste:

```elixir
# test/some_api_client_test.exs
defmodule SomeAPIClientTest do
  use ExUnit.Case
  import Mox

  # Isso diz ao Mox para verificar se este mock foi chamado como esperado
  setup :verify_on_exit!

  test "obtém dados da API" do
    # Configura a resposta do mock
    expect(HTTPClientMock, :get, fn _url -> {:ok, "Resposta Mockada"} end)
    
    assert SomeAPIClient.get_data() == "Resposta Mockada"
  end
end
```

Ao executar `mix test`, essa configuração permite isolar seus testes de unidade de dependências externas reais, focando no comportamento do seu próprio código. Esse padrão garante que seus testes sejam executados rapidamente e permaneçam confiáveis, independentemente do status do serviço externo ou da conectividade com a internet.
