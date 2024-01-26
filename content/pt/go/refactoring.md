---
title:                "Refatoração"
date:                  2024-01-26T01:36:49.261897-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refatoração"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/refactoring.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Refatoração é o processo de reestruturar o código de computador existente sem alterar seu comportamento externo. Programadores fazem isso para melhorar atributos não funcionais do software, como legibilidade e capacidade de manutenção, o que pode tornar o código mais fácil de entender, reduzir a complexidade e ajudar a identificar bugs mais facilmente.

## Como Fazer:
Vamos mergulhar em um exemplo simples de refatoração de código em Go. Vamos pegar um trecho que calcula a média de uma fatia de números e refatorá-lo para maior clareza e reutilização.

Código original:
```Go
package main

import "fmt"

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    var sum float64
    for _, num := range numbers {
        sum += num
    }
    average := sum / float64(len(numbers))
    fmt.Println("Média:", average)
}
```

Código refatorado:
```Go
package main

import "fmt"

// CalculateAverage recebe uma fatia de float64 e retorna a média.
func CalculateAverage(numbers []float64) float64 {
    sum := 0.0
    for _, num := range numbers {
        sum += num
    }
    return sum / float64(len(numbers))
}

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    average := CalculateAverage(numbers)
    fmt.Println("Média:", average)
}
```

No código refatorado, extraímos a lógica que calcula a média para uma função separada chamada `CalculateAverage`. Isso torna a função `main` mais concisa e a lógica de cálculo da média reutilizável e testável.

## Aprofundamento
Refatorar código não é um conceito moderno; precede o uso generalizado de computadores. A prática provavelmente começou no domínio da engenharia mecânica ou mesmo antes. Em software, tornou-se mais formalizada com o advento da programação orientada a objetos e programação extrema (XP) nos anos 90, notavelmente influenciada pelo livro seminal de Martin Fowler "Refactoring: Improving the Design of Existing Code."

Existem inúmeras técnicas de refatoração, desde renomear variáveis para clareza até padrões mais complexos como extrair métodos ou classes. O chave é fazer pequenas mudanças incrementais que não modificam a funcionalidade do software, mas melhoram a estrutura interna.

Ao usar Go, a refatoração pode ser direta devido à simplicidade da linguagem e à poderosa biblioteca padrão. No entanto, ainda é importante ter um bom conjunto de testes unitários para garantir que a refatoração não introduza bugs. Ferramentas como `gorename` e `gofmt` ajudam a automatizar alguns dos processos, e as IDEs geralmente têm suporte integrado à refatoração.

Além da refatoração manual, existem algumas ferramentas de refatoração de código automatizadas disponíveis para Go, como as ferramentas de refatoração do GoLand e o Go Refactor. Embora possam acelerar o processo, elas não substituem o entendimento do código e a realização de mudanças consideradas.

## Veja Também
 - [Refatoração em Go: Simples é Bonito](https://go.dev/blog/slices)
 - [Go Efetivo: Refatoração com Interfaces](https://go.dev/doc/effective_go#interfaces)
 - [Página de Refatoração de Martin Fowler](https://refactoring.com/)
 - [Ferramentas de Refatoração GoLand](https://www.jetbrains.com/go/features/refactorings/)