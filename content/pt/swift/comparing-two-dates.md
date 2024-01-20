---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Comparar duas datas significa verificar se uma data é mais antiga, mais recente ou igual a outra. Programadores fazem isso frequentemente para manipular, organizar dados em seu software ou realizar cálculos que dependem da passagem do tempo.

## Como Fazer:

No Swift, você pode comparar duas instâncias `Date` usando os operadores de comparação padrão. Aqui está um exemplo:

```Swift
let dataAntiga = Date(timeIntervalSince1970: 60)
let dataRecente = Date(timeIntervalSince1970: 3600)

if dataAntiga < dataRecente {
  print("A dataAntiga vem antes da dataRecente")
} else if dataAntiga > dataRecente {
  print("A dataAntiga vem depois da dataRecente")
} else {
  print("As datas são iguais")
}
```
Se você rodar este código, você verá a seguinte saída: "A dataAntiga vem antes da dataRecente"

## Mergulho Profundo 

Antigamente, a comparação de datas era uma tarefa complicada devido a questões como fusos horários e bissextos. Swift simplifica isso por meio da classe Date e TimeInterval.

Uma alternativa ao uso direto dos operadores de comparação seria usar os métodos `compare(_:)` ou `isEqual(to:)` da classe Date. Esses métodos fornecem uma abordagem mais semântica, mas são um pouco mais verbosos e menos comuns em comparação ao uso dos operadores de comparação.

Internamente, a comparação de datas no Swift é possível porque a classe Date armazena a data e a hora como um ponto único na linha do tempo, em segundos desde 1 de janeiro de 2001.

## Veja Também 

1. [Documentação oficial do Swift - Date](https://developer.apple.com/documentation/foundation/date)
2. [Comparing Dates in Swift](https://useyourloaf.com/blog/comparing-dates-in-swift/)
3. [Swift by Sundell - Working with dates](https://www.swiftbysundell.com/basics/dates/)