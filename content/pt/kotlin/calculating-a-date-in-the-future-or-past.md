---
title:                "Calculando uma data no futuro ou no passado"
html_title:           "Kotlin: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Calcular uma data no futuro ou passado é uma tarefa comum para programadores. Isso envolve determinar uma data específica com base em uma data inicial e um determinado número de unidades de tempo, como dias ou meses. Programadores geralmente calculam datas para automatizar tarefas e criar funcionalidades como lembretes e agendamentos.

## Como fazer:
```kotlin
val initialDate = LocalDate.of(2021, 8, 1)
val futureDate = initialDate.plusDays(5)
val pastDate = initialDate.minusMonths(3)
println("Data inicial: $initialDate")
println("Data no futuro: $futureDate")
println("Data no passado: $pastDate")
```
Output:
Data inicial: 2021-08-01
Data no futuro: 2021-08-06
Data no passado: 2021-05-01

## Mergulho profundo:
Calcular datas no passado ou futuro tem sido uma tarefa importante desde os primórdios da computação. A linguagem de programação COBOL, lançada em 1959, possuía uma função integrada para calcular datas. Hoje, existem muitas outras linguagens de programação que suportam essa funcionalidade. Além do método apresentado acima, também é possível utilizar outras bibliotecas e funções para realizar esses cálculos em Kotlin.

## Veja também:
- [Documentação oficial do Kotlin sobre manipulação de datas](https://kotlinlang.org/docs/datetime.html)
- [Como calcular uma data no futuro ou passado em Java](https://www.baeldung.com/java-dates-in-future-past)
- [Outras bibliotecas para manipulação de datas em Kotlin](https://www.bezkoder.com/kotlin-date-time-getting-started/)