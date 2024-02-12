---
title:                "Refatoração"
aliases:
- /pt/elixir/refactoring.md
date:                  2024-01-26T01:17:51.773148-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refatoração"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/refactoring.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Refatoração é o processo de reestruturação do código existente sem alterar seu comportamento externo, visando melhorar atributos não funcionais como legibilidade e manutenibilidade. Programadores fazem isso para tornar o código mais limpo, fácil de entender e mais eficiente, facilitando futuras atualizações e reduzindo o risco de bugs.

## Como fazer:
Vamos arrumar um padrão comum em Elixir. Vamos refatorar uma função `calculate_stats` que faz mais do que deveria, dividindo-a em partes menores e reutilizáveis.

```elixir
defmodule Stats do
  # Código original, sem refatoração
  def calculate_stats(data) do
    total = Enum.sum(data)
    count = Enum.count(data)
    mean = total / count
    {mean, total}
  end
  
  # Código refatorado
  def calculate_mean(data), do: Enum.sum(data) / Enum.count(data)
  
  def calculate_total(data), do: Enum.sum(data)
  
  def calculate_stats_refactored(data) do
    mean = calculate_mean(data)
    total = calculate_total(data)
    {mean, total}
  end
end

# Saída de Exemplo
# Antes da Refatoração
Stats.calculate_stats([1, 2, 3])
# => {2.0, 6}

# Após a Refatoração
Stats.calculate_stats_refactored([1, 2, 3])
# => {2.0, 6}
```
Como você pode ver, a saída permanece a mesma, mas agora temos funções modulares que podem ser reutilizadas e testadas independentemente.

## Aprofundamento
Refatoração não é um conceito novo; tem sido uma parte crucial da programação desde os primeiros dias do desenvolvimento de software. Obras notáveis, como "Refactoring: Improving the Design of Existing Code" de Martin Fowler, fornecem práticas fundamentais para refatoração com insights sobre quando e como aplicá-las.

Alternativas à refatoração manual incluem ferramentas automatizadas de análise de código, que podem sugerir ou até mesmo realizar refatorações. No entanto, as ferramentas automatizadas podem não compreender sempre o contexto completo do código e podem perder sutilezas que um revisor humano captaria.

Detalhes de implementação em Elixir especificamente incluem entender o paradigma funcional e alavancar correspondência de padrões, cláusulas de guarda e o operador pipe para escrever código claro e conciso. Por exemplo, a refatoração muitas vezes envolve converter funções complexas de estilo imperativo em funções menores, componíveis, que seguem a preferência do Elixir por imutabilidade e operações sem efeitos colaterais.

## Veja Também
Para mais técnicas de refatoração específicas de Elixir:

- [Guias oficiais do Elixir](https://elixir-lang.org/getting-started/)
- ["Refactoring: Improving the Design of Existing Code" de Martin Fowler](https://martinfowler.com/books/refactoring.html), para princípios gerais que podem ser aplicados ao Elixir.
- [Credo, uma ferramenta de análise estática de código para Elixir](https://github.com/rrrene/credo) que incentiva melhores práticas.
- [Faixa Elixir do Exercism](https://exercism.org/tracks/elixir), para exercícios práticos que muitas vezes envolvem refatoração.
