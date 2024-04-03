---
date: 2024-01-26 03:37:05.480226-07:00
description: "Refatora\xE7\xE3o \xE9 o processo de reestruturar o c\xF3digo de computador\
  \ existente sem alterar seu comportamento externo. Programadores refatoram para\
  \ melhorar\u2026"
lastmod: '2024-03-13T22:44:47.103652-06:00'
model: gpt-4-0125-preview
summary: "Refatora\xE7\xE3o \xE9 o processo de reestruturar o c\xF3digo de computador\
  \ existente sem alterar seu comportamento externo."
title: "Refatora\xE7\xE3o"
weight: 19
---

## Como:
Vamos ver um exemplo de refatoração de um método Ruby que calcula a soma dos quadrados.

**Antes da Refatoração:**
```ruby
def sum_of_squares(numbers)
  sum = 0
  numbers.each do |number|
    square = number * number
    sum += square
  end
  sum
end

puts sum_of_squares([1, 2, 3])  # Saída: 14
```

**Após a Refatoração:**
```ruby
def sum_of_squares(numbers)
  numbers.map { |number| number**2 }.sum
end

puts sum_of_squares([1, 2, 3])  # Saída: 14
```

A versão refatorada utiliza os Enumeráveis do Ruby para expressar a mesma lógica de forma mais sucinta e clara. O método `map` transforma cada elemento e `sum` agrega seus valores, removendo a necessidade de gerenciamento manual de loop e atribuição de variáveis.

## Aprofundamento
Refatoração tem um rico contexto histórico, remontando às práticas iniciais no desenvolvimento de software. Menções iniciais podem ser rastreadas até a década de 1990, com contribuições significativas de Martin Fowler em seu livro "Refactoring: Improving the Design of Existing Code", onde ele fornece um catálogo de padrões para refatoração. Desde então, a refatoração se tornou um pilar das práticas de desenvolvimento ágil.

Quando falamos sobre alternativas à refatoração, precisamos considerar uma abordagem diferente como 'Reescrever', onde você substitui o sistema antigo em partes ou inteiramente ou adotar práticas como 'Revisões de Código' e 'Programação em Pares' para melhorar gradualmente a qualidade do código. No entanto, estas não são substituições para a refatoração; elas complementam o processo.

Em termos de implementação, Ruby oferece uma sintaxe excelente e expressiva que muitas vezes resulta em código mais curto e legível após a refatoração. Princípios chave incluem DRY (Don't Repeat Yourself - Não se Repita), usar nomes significativos, manter métodos curtos e focados em uma única tarefa, e usar efetivamente o módulo Enumerable do Ruby, como visto no exemplo acima. Ferramentas automatizadas como o RuboCop também podem ajudar programadores a identificar pontos no código que poderiam beneficiar de refatoração.

## Veja Também
Para se aprofundar mais em refatoração em Ruby, confira estes recursos:

- O livro seminal de Martin Fowler: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- Guia de estilo do Ruby para escrever um código mais limpo: [The Ruby Style Guide](https://rubystyle.guide/)
- RuboCop, um analisador estático de código (linter) e formatador: [Repositório GitHub do RuboCop](https://github.com/rubocop/rubocop)
