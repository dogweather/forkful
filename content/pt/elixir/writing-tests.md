---
title:    "Elixir: Escrevendo testes"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/writing-tests.md"
---

{{< edit_this_page >}}

# Por que escrever testes é importante em Elixir?

Escrever testes é uma parte essencial do processo de programação em qualquer linguagem, e o Elixir não é exceção. Os testes nos permitem garantir que o nosso código funcione corretamente e sem erros antes mesmo de ser executado. Além disso, eles são uma forma de documentar e validar o código, facilitando a manutenção e colaboração em projetos.

## Como escrever testes em Elixir

Elixir possui uma biblioteca de testes padrão chamada ExUnit, que nos permite escrever e executar testes de forma eficiente. Para começar, devemos criar um módulo de testes em nosso arquivo de código-fonte, usando o nome do módulo original com o sufixo "_test". Por exemplo, se nosso arquivo se chama "calculadora.ex", nosso módulo de testes será "calculator_test.ex". Dentro do módulo, usamos a diretiva "use ExUnit.Case" para importar os recursos necessários para escrever os testes.

```elixir
defmodule CalculatorTest do
  use ExUnit.Case
```

Dentro do módulo de testes, definimos funções de teste usando a macro "def", seguida pelo nome do teste e o operador "->". Dentro do corpo do teste, usamos as funções da biblioteca ExUnit para realizar as asserções e verificar se o resultado está correto.

```elixir
def test_addition do
  assert Calculator.add(2, 3) == 5
end
```

Para executar os testes, basta rodar o comando "mix test". Se todos os testes forem aprovados, veremos uma saída como a seguinte:

```
Finished in 0.1 seconds
1 test, 0 failures
```

## Uma análise mais profunda sobre escrever testes

Além dos testes unitários, que verificam o comportamento de funções individuais, também podemos escrever testes de integração em Elixir. Esses testes são responsáveis por testar o fluxo completo de uma funcionalidade e garantir que todas as partes do sistema funcionem em conjunto da maneira esperada.

Outro conceito importante em testes em Elixir é a ideia de imutabilidade. Por ser uma linguagem funcional, o Elixir possui variáveis imutáveis, o que significa que não podemos alterar o valor delas após a atribuição inicial. Isso pode tornar a escrita de testes um pouco diferente do que em linguagens imperativas, mas também traz benefícios, como testes mais previsíveis e menos propensos a erros.

Em resumo, escrever testes é crucial para garantir a qualidade e a robustez do nosso código em Elixir. Além disso, nos ajuda a documentar e validar nossas funcionalidades, tornando o desenvolvimento mais eficiente e colaborativo.

## Ver também

- [Documentação oficial do ExUnit](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Tutoriais de testes em Elixir](https://medium.com/tag/elixir-testing)
- [Guia de melhores práticas de testes em Elixir](https://medium.com/coding-hints/elixir-unit-test-best-practices-e63f1aa0ce2d)