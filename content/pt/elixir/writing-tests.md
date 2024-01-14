---
title:    "Elixir: Escrevendo testes"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por que escrever testes de código em Elixir?

Escrever testes é uma prática importante em qualquer linguagem de programação, pois ajuda a garantir que o código produzido esteja funcionando corretamente e não cause problemas futuros. Com a linguagem Elixir, não é diferente. Além disso, escrever testes também facilita na detecção de erros e na realização de modificações no código sem quebrar o funcionamento do programa.

## Como escrever testes em Elixir

Para escrever testes em Elixir, utilizamos o módulo de Asseções ([ExUnit](https://hexdocs.pm/ex_unit/ExUnit.html)). Ele fornece diversas funções que nos permite criar testes de uma maneira rápida e eficiente. Por exemplo, para testar uma função que soma dois números, podemos escrever o seguinte código:

```Elixir
defmodule Calculator do
  def soma(a, b) do
    a + b
  end
end

defmodule CalculatorTest do
  use ExUnit.Case

  test "soma dois números" do
    assert Calculator.soma(2, 3) == 5
  end
end
```
Ao executar o teste, é esperado que a saída seja `* 1 test, 0 failures*`, indicando que o teste passou com sucesso.

Podemos também utilizar a função `assert_raise` para testar se uma determinada função lança uma exceção, por exemplo:

```Elixir
defmodule CalculatorTest do
  use ExUnit.Case

  test "divisão de dois números" do
    assert_raise ArithmeticError, fn -> Calculator.divisao(4, 0) end
  end
end
```
Nesse caso, o teste será considerado bem sucedido se a função `divisao` lançar uma exceção do tipo `ArithmeticError`.

## Profundidade dos testes em Elixir

Além das funções disponíveis no módulo `ExUnit`, também podemos realizar testes mais aprofundados utilizando os conceitos de [testes de unidade](https://pt.wikipedia.org/wiki/Teste_unit%C3%A1rio) e [testes de integração](https://pt.wikipedia.org/wiki/Teste_de_integra%C3%A7%C3%A3o). Os testes de unidade garantem que cada função do código está funcionando corretamente de forma isolada, enquanto os testes de integração verificam se as funções estão trabalhando em conjunto como esperado.

Outra prática importante é a utilização do padrão de desenvolvimento orientado a testes ([TDD](https://pt.wikipedia.org/wiki/Test-driven_development)). Com TDD, escrevemos os testes antes mesmo de desenvolver a funcionalidade desejada, garantindo uma maior cobertura de testes e evitando a criação de código desnecessário.

## Veja também

- [Documentação oficial do ExUnit](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Vídeo sobre TDD em Elixir (em inglês)](https://www.youtube.com/watch?v=HfBC_a6LCtY)
- [Livro sobre TDD em Elixir (em inglês)](https://pragprog.com/book/lmelixir/programming-elixir-1-3)