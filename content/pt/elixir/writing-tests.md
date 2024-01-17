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

## O que e por que?

Escrever testes na programação é um processo de verificação e validação do código para garantir que o mesmo esteja funcionando corretamente. Os programadores utilizam testes para detectar bugs e garantir a qualidade do código.

## Como fazer:

Elixir tem uma estrutura de testes embutida chamada ExUnit, que torna o processo de escrever testes fácil e eficiente. Abaixo está um exemplo de como escrever testes em Elixir:

```Elixir
defmodule CalculatorTest do
  use ExUnit.Case

  test "addition" do
    assert Calculator.add(2, 3) == 5
  end

  test "multiplication" do
    assert Calculator.multiply(2, 3) == 6
  end
end
```

Neste exemplo, estamos testando as funções de adição e multiplicação de uma calculadora simples. A estrutura de testes começa com `defmodule` e `use ExUnit.Case`, seguido de `test` e uma descrição do teste. Dentro de cada teste, usamos a função `assert` para verificar se o resultado esperado é igual ao resultado real.

## Mergulho profundo:

A prática de escrever testes é conhecida como desenvolvimento guiado por testes (TDD), e foi introduzida pelo programador Kent Beck no início dos anos 2000. O objetivo do TDD é criar testes primeiro e, em seguida, escrever o código necessário para passar nesses testes. Isso ajuda a garantir que o código funcione como esperado e evita possíveis falhas futuras.

Além do ExUnit, existem outras ferramentas de teste disponíveis em Elixir, como Hound para testes de aplicativos web e Mox para simular módulos em ambiente de teste. É importante encontrar a ferramenta que melhor se adapte às necessidades de cada projeto.

## Veja também:

- [Documentação do ExUnit](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Vídeo sobre TDD com Elixir](https://www.youtube.com/watch?v=3QvOVol7mLE)
- [Artigo sobre TDD com Elixir](https://medium.com/@sachinbhate/test-driven-development-with-elixir-a-simple-guide-952c6ce03b32)