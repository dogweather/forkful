---
date: 2024-01-20 17:32:24.853859-07:00
description: "Como Fazer: A capacidade de calcular datas \xE9 essencial desde o in\xED\
  cio da computa\xE7\xE3o. Algoritmos para c\xE1lculo de datas t\xEAm ra\xEDzes hist\xF3\
  ricas, como o\u2026"
lastmod: '2024-04-05T22:51:00.174141-06:00'
model: gpt-4-1106-preview
summary: "A capacidade de calcular datas \xE9 essencial desde o in\xEDcio da computa\xE7\
  \xE3o."
title: Calculando uma data no futuro ou passado
weight: 26
---

## Como Fazer:
```Swift
import Foundation

// Data atual
let hoje = Date()

// Calendário padrão
let calendario = Calendar.current

// Adicionando 5 dias à data atual
if let dataFutura = calendario.date(byAdding: .day, value: 5, to: hoje) {
    print("Daqui a 5 dias: \(dataFutura)")
} else {
    print("Erro ao calcular a data futura.")
}

// Subtraindo 30 dias da data atual
if let dataPassada = calendario.date(byAdding: .day, value: -30, to: hoje) {
    print("Há 30 dias: \(dataPassada)")
} else {
    print("Erro ao calcular a data passada.")
}
```
Sample output:
```
Daqui a 5 dias: 2023-04-12 14:00:00 +0000
Há 30 dias: 2023-03-08 14:00:00 +0000
```

## Mergulho Profundo
A capacidade de calcular datas é essencial desde o início da computação. Algoritmos para cálculo de datas têm raízes históricas, como o Algoritmo de Gauss para determinar a data da Páscoa. Em Swift, a `Foundation` nos oferece `DateComponents` para manipular elementos de datas facilmente. 

Alternativamente, é possível utilizar timestamps UNIX (representação de tempo em segundos desde 1970) para efetuar cálculos, mas isso é menos legível e mais suscetível a erros com fusos horários e regras de calendário.

Em termos de implementação, cuidado com fusos horários e a mudança para horário de verão, que podem influenciar o cálculo de datas. O Swift lida bem com esses detalhes se usarmos `Calendar` e `DateComponents`.

## Ver Também
- Documentação da Apple sobre `DateComponents`: [Documentação DateComponents](https://developer.apple.com/documentation/foundation/datecomponents)
- Tutorial de Swift sobre manipulação de datas: [Ray Wenderlich's Date and Time Tutorial](https://www.raywenderlich.com/5817-background-modes-tutorial-getting-started)
