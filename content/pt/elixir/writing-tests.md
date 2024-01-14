---
title:                "Elixir: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Elixir?

Testes são uma parte essencial de qualquer processo de desenvolvimento de software. Eles garantem que nosso código funcione como esperado e nos dão confiança para fazer mudanças sem medo de que algo quebre. Em Elixir, a escrita de testes também nos permite aproveitar ao máximo o poderoso sistema de concorrência da linguagem.

## Como escrever testes em Elixir 

Para escrever testes em Elixir, precisamos primeiro criar um módulo com o sufixo `_test` no nome. Por exemplo, se tivermos um módulo chamado `Calculator`, nosso módulo de teste seria chamado de `CalculatorTest`. Em seguida, podemos usar a macro `deftest` para definir nossos testes e a macro `assert` para verificar se o resultado é o esperado.

```
defmodule CalculatorTest do
  use ExUnit.Case

  test "sum" do
    result = Calculator.sum(2, 3)
    assert result == 5
  end
end
```

Para executar nossos testes, podemos usar o comando `mix test` no terminal. Isso executará todos os testes em nosso projeto e nos informará se há algum caso falhado.

## Mergulho profundo em testes

Além da macro `assert`, o Elixir também possui outras macros úteis para escrever testes, como `assert_raise` para testar se uma determinada exceção é levantada e `assert_receive` para verificar se um processo envia uma mensagem específica. Também podemos usar `setup` e `teardown` para definir comportamentos antes e depois de cada teste, respectivamente.

Outra vantagem de escrever testes em Elixir é a possibilidade de executar testes em paralelo, usando a diretiva `concurrent: true` na definição do nosso módulo de teste. Isso permite que os testes sejam executados de forma mais rápida, tornando nosso processo de desenvolvimento mais eficiente.

## Veja também

- [Documentação oficial de testes em Elixir](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Artigo sobre testes em Elixir da Thoughtbot](https://thoughtbot.com/blog/unit-testing-and-pattern-matching-with-elixir)
- [Tutorial de testes em Elixir do Elixir School](https://elixirschool.com/en/lessons/basics/testing/)

Agora que você sabe como escrever testes em Elixir, não deixe de incluí-los em seus projetos. Com testes bem escritos, podemos ter a certeza de que nosso código está funcionando corretamente e também facilitar a manutenção e extensão do mesmo.