---
title:    "Swift: Calculando uma data no futuro ou passado"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por que calcular uma data no futuro ou passado?

Há muitas razões pelas quais alguém pode querer calcular uma data no futuro ou passado em programação Swift. Isso pode ser útil para criar um calendário, agendar eventos ou até mesmo para fins de controle de estoque.

## Como fazer:

Para calcular uma data no futuro ou passado, primeiro precisamos da data atual e, em seguida, usaremos o método `Calendar.date(byAdding:value:to:)` para adicionar ou subtrair o valor desejado. Aqui está um exemplo de código:

```Swift
// Declare uma data atual
let hoje = Date()

// Adicione 5 dias à data atual
let dataNoFuturo = Calendar.current.date(byAdding: .day, value: 5, to: hoje)

// Subtraia 2 meses da data atual
let dataPassada = Calendar.current.date(byAdding: .month, value: -2, to: hoje)

// Imprimir o resultado
print("Data no futuro: \(dataNoFuturo)")
print("Data passada: \(dataPassada)")
```

A saída do código acima seria:

```
Data no futuro: Optional(2021-08-21 13:30:32 +0000)
Data passada: Optional(2021-04-20 13:30:32 +0000)
```

## Mergulho Profundo:

A classe `Calendar` em Swift nos permite trabalhar com datas de uma maneira mais avançada. É possível especificar o tipo de calendário e localização para os cálculos, além de adicionar ou subtrair outros componentes, como horas, minutos e segundos.

Também é importante destacar que o método `date(byAdding:value:to:)` retorna um valor opcional, o que significa que a data resultante pode ser nula se o cálculo não puder ser feito.

## Veja também:

- [Documentação da classe Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [Swift Playgrounds: Construindo um Calendário](https://developer.apple.com/documentation/swift_playground_support/building_a_calendar_with_swift_playgrounds)