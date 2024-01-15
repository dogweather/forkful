---
title:                "Escrevendo testes"
html_title:           "Elixir: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Elixir?

Ao escrever testes em Elixir, os desenvolvedores garantem que seu código está funcionando corretamente e que quaisquer alterações feitas no futuro não quebrarão o código existente.

## Como fazer

Um teste básico em Elixir pode ser definido da seguinte forma:

```Elixir
defmodule MathTest do
  use ExUnit.Case
  
  test "soma de dois números" do
    assert 2 + 3 == 5
  end
end
```

Nesse exemplo, estamos criando um módulo `MathTest` que utiliza a estrutura de teste do Elixir `ExUnit.Case` e criando um teste simples que verifica a soma de dois números.

Executar esse teste é simples, basta rodar o seguinte comando no terminal:

```
mix test
```

Isso irá executar todos os testes presentes no diretório `test` do seu projeto. Caso queira executar apenas o teste que acabamos de criar, é possível passar o nome do arquivo como argumento:

```
mix test test/math_test.exs
```

O resultado esperado seria algo como:

```
  1) test soma de dois números (MathTest)
      test/math_test.exs:5
      Assertion with == failed
      code: 2 + 3 == 5
      lhs:  5
      rhs:  6
      stacktrace:
        test/math_test.exs:6: (test)
```

Essa saída nos informa que o teste falhou, pois o resultado da expressão `2 + 3` é diferente de 5. Isso nos permite identificar rapidamente e corrigir o erro.

## Profundidade na escrita de testes

Ao escrever testes em Elixir, é importante ter em mente que devemos testar não apenas o resultado esperado, mas também possíveis erros ou comportamentos inesperados. Isso garantirá que nosso código esteja mais robusto e que possíveis problemas sejam detectados e corrigidos antes de chegarem em produção.

Além disso, é possível utilizar a estrutura de testes do Elixir para testar funções que utilizam processos concorrentes. Isso nos permite garantir que nosso código seja capaz de lidar com múltiplos processos e dados compartilhados sem causar problemas.

## Veja também

- Documentação oficial da estrutura de teste do Elixir: https://hexdocs.pm/ex_unit/ExUnit.html
- Artigo sobre TDD em Elixir: https://www.obiefernandez.com/posts/tdd-in-elixir
- Exemplo de projeto com testes em Elixir: https://github.com/elixirschool/elixir-survey-2016