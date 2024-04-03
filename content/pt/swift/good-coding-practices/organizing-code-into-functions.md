---
date: 2024-01-26 01:16:10.636724-07:00
description: "Como fazer: Imagine uma tarefa: calcular a m\xE9dia de um array. Sem\
  \ fun\xE7\xF5es, voc\xEA colocaria tudo no main. Com fun\xE7\xF5es, voc\xEA faria\
  \ assim."
lastmod: '2024-03-13T22:44:46.925845-06:00'
model: gpt-4-0125-preview
summary: Imagine uma tarefa.
title: "Organizando o c\xF3digo em fun\xE7\xF5es"
weight: 18
---

## Como fazer:
Imagine uma tarefa: calcular a média de um array. Sem funções, você colocaria tudo no main. Com funções, você faria assim:

```swift
func calculateAverage(of numbers: [Double]) -> Double {
    let sum = numbers.reduce(0, +)
    return numbers.isEmpty ? 0 : sum / Double(numbers.count)
}

// Uso
let scores = [92.5, 88.75, 99.0, 70.5]
let averageScore = calculateAverage(of: scores)
print("A média das notas é \(averageScore)")
```

A saída de amostra seria: 
```
A média das notas é 87.6875
```

## Mergulho Profundo
Historicamente, à medida que a programação se tornou complexa, as funções se tornaram uma pedra angular para gerenciar a complexidade. Alternativas incluem codificação inline e copiar e colar código (código espaguete) – agora amplamente considerado má prática. Em Swift, funções são cidadãos de primeira classe; elas podem ser atribuídas a variáveis, passadas como argumentos e retornadas de outras funções, tornando o código mais modular e flexível.

Em termos de implementação, projete suas funções para fazer bem uma única coisa. Aspire por funções com um propósito claro e um nome que reflita isso. Observe a contagem de parâmetros — muitos e você provavelmente está fazendo demais. Tratamento de erro? Considere funções que lançam exceções e lide com problemas de maneira graciosa. Lembre-se: Swift é tudo sobre legibilidade e facilidade de manutenção.

## Veja Também
- [Guia da Linguagem de Programação Swift - Funções](https://docs.swift.org/swift-book/LanguageGuide/Functions.html)
- [Guia de Estilo Swift do Ray Wenderlich](https://github.com/raywenderlich/swift-style-guide)
- [Refatoração: Melhorando o Design do Código Existente, de Martin Fowler](https://martinfowler.com/books/refactoring.html)
