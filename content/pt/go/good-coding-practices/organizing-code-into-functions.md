---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:30.742805-07:00
description: "Como Fazer: Em Go, voc\xEA define uma fun\xE7\xE3o usando a palavra-chave\
  \ `func`, seguida pelo nome da fun\xE7\xE3o, par\xE2metros (se houver) e o tipo\
  \ de retorno. Vamos\u2026"
lastmod: '2024-03-13T22:44:46.066833-06:00'
model: gpt-4-0125-preview
summary: "Em Go, voc\xEA define uma fun\xE7\xE3o usando a palavra-chave `func`, seguida\
  \ pelo nome da fun\xE7\xE3o, par\xE2metros (se houver) e o tipo de retorno."
title: "Organizando c\xF3digo em fun\xE7\xF5es"
weight: 18
---

## Como Fazer:
Em Go, você define uma função usando a palavra-chave `func`, seguida pelo nome da função, parâmetros (se houver) e o tipo de retorno. Vamos ilustrar com um exemplo simples:

```go
package main

import "fmt"

// define uma função para calcular a soma de dois números
func addNumbers(a int, b int) int {
    return a + b
}

func main() {
    sum := addNumbers(5, 7)
    fmt.Println("A soma é:", sum)
    // Saída: A soma é: 12
}
```

Funções também podem retornar múltiplos valores, o que é uma característica única comparada a muitas outras linguagens. Veja como você pode aproveitar isso:

```go
// define uma função para trocar dois números
func swap(a, b int) (int, int) {
    return b, a
}

func main() {
    x, y := swap(10, 20)
    fmt.Println("x, y após troca:", x, y)
    // Saída: x, y após troca: 20 10
}
```

Você também pode definir funções com um número variável de argumentos usando a reticências `...` antes do tipo de parâmetro. Isso é útil para criar funções flexíveis:

```go
// define uma função para calcular a soma de um número desconhecido de inteiros
func sum(numbers ...int) int {
    total := 0
    for _, number := range numbers {
        total += number
    }
    return total
}

func main() {
    total := sum(1, 2, 3, 4, 5)
    fmt.Println("O total é:", total)
    // Saída: O total é: 15
}
```

## Aprofundando
O conceito de organizar o código em funções não é peculiar ao Go — é um princípio fundamental da programação. No entanto, o Go introduz certas convenções e capacidades que distinguem sua gestão de funções. Por exemplo, a capacidade de retornar múltiplos valores das funções é relativamente única e pode levar a um código mais limpo e compreensível, especialmente ao lidar com operações que tradicionalmente poderiam requerer o uso de ponteiros ou tratamento de exceções.

Além disso, o suporte do Go a funções de primeira classe — funções que podem ser passadas como argumentos para outras funções, retornadas como valores de funções e atribuídas a variáveis — realça o suporte do idioma para padrões de programação funcional. Esta característica é particularmente útil na criação de funções de alta ordem que manipulam ou combinam outras funções.

No entanto, é essencial estar atento à "lei dos rendimentos decrescentes" ao organizar o código em funções. A sobre-modularização pode levar a uma abstração excessiva, tornando o código mais difícil de entender e manter. Além disso, embora a abordagem simplista do Go ao tratamento de erros (retornando erros como valores de retorno normais) incentive uma propagação limpa de erros por múltiplas camadas de chamadas de função, isso pode levar a um código de tratamento de erros repetitivo. Alternativas como frameworks de tratamento de erros ou a adoção da abordagem de "try-catch" de outras linguagens (embora não suportada nativamente) através de implementações de pacotes podem, às vezes, oferecer soluções mais elegantes dependendo do caso de uso.

A decisão de quão extensivamente utilizar funções e modularização em Go deve equilibrar a necessidade de abstração, manutenibilidade, performance e tratamento de erros legível, aproveitando ao máximo as características diretas, ainda que poderosas, do Go.
