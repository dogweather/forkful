---
title:                "Escrevendo testes"
html_title:           "Arduino: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?

Escrever testes é criar código para verificar se outro código funciona como esperado. Programadores testam para evitar bugs, garantir qualidade e facilitar manutenção.

## Como Fazer:

```elixir
# Dependência no mix.exs
{:ex_unit, "~> 1.11", only: :test}

# Definição do teste em test/some_module_test.exs
defmodule SomeModuleTest do
  use ExUnit.Case
  doctest SomeModule

  test "soma de dois números" do
    assert SomeModule.soma(2, 3) == 5
  end
end
```

Saída esperada após rodar `mix test`:
```
...

Finished in 0.05 seconds
1 test, 0 failures

Randomized with seed 54321
...
```

## Aprofundando:

No mundo Elixir, o ExUnit é o framework mais comum para testes, surgindo com a linguagem. Há alternativas como o ESpec, inspirado no RSpec do Ruby. Testes podem ser unitários, de integração ou de aceitação, variando no escopo e detalhe. O Elixir incentiva testes por ser uma linguagem funcional onde funções puras são facilmente testáveis.

## Veja Também:

- Documentação oficial do ExUnit: https://hexdocs.pm/ex_unit/ExUnit.html
- ESpec, para uma abordagem estilo RSpec: https://github.com/antonmi/espec
- Blog sobre TDD em Elixir: https://elixir-lang.org/blog/2020/12/21/the-joy-of-test-driven-development/
