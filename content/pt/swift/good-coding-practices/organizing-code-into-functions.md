---
title:                "Organizando o código em funções"
aliases: - /pt/swift/organizing-code-into-functions.md
date:                  2024-01-26T01:16:10.636724-07:00
model:                 gpt-4-0125-preview
simple_title:         "Organizando o código em funções"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Agrupar código em funções é decompor tarefas em pedaços reutilizáveis. Isso torna o código limpo, menos propenso a erros e mais fácil de depurar ou refatorar.

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
