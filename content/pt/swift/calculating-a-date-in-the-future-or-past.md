---
title:                "Swift: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calcular datas no futuro ou passado é uma tarefa essencial em muitos aplicativos, seja para agendar eventos, criar lembretes ou realizar operações com base em datas específicas. Com as ferramentas certas, essa tarefa pode ser feita de forma rápida e eficiente em Swift.

## Como fazer

Para calcular uma data no futuro ou passado usando Swift, é necessário primeiro importar o framework "Foundation". Em seguida, é possível utilizar o tipo de dados `Date` para armazenar uma data e os métodos `addingTimeInterval` ou `addingDateComponents` para fazer os cálculos desejados.

### Exemplo 1: Calcular uma data no futuro ou passado em segundos

```
import Foundation

let dataAtual = Date()
let segundos = 3600 // 1 hora

let dataCalculada = dataAtual.addingTimeInterval(TimeInterval(segundos)) // Adiciona 1 hora à data atual

// Saída: 2021-07-16 22:30:00 +0000
print(dataCalculada)
```

### Exemplo 2: Calcular uma data no futuro ou passado com base em componentes de data personalizados

```
import Foundation

let dataAtual = Date()
var dataComponentes = DateComponents()

dataComponentes.year = 2022
dataComponentes.month = 12
dataComponentes.day = 31
dataComponentes.hour = 23
dataComponentes.minute = 59
dataComponentes.second = 59

let dataCalculada = Calendar.current.date(byAdding: dataComponentes, to: dataAtual) // Adiciona os componentes à data atual

// Saída: 2022-12-31 23:59:59 +0000
print(dataCalculada)
```

## Mergulho profundo

A classe `Calendar` oferece uma grande variedade de métodos que permitem um controle preciso sobre as datas, como a possibilidade de especificar diferentes calendários, fusos horários e até mesmo lidar com problemas de horário de verão. Além disso, o uso de componentes de data permite que o cálculo de datas leve em consideração diferentes unidades de tempo, como anos, meses e dias.

## Veja também

- [Documentação oficial do framework Foundation](https://developer.apple.com/documentation/foundation)
- [Tutorial: Manipulação de datas em Swift](https://www.raywenderlich.com/3118345-date-manipulation-in-swift-explained-with-examples)
- [Artigo: Recursos avançados de data e hora em Swift](https://medium.com/better-programming/swift-date-time-advanced-usage-and-issues-25b71954cde6)