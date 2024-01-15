---
title:                "Calculando uma data no futuro ou passado"
html_title:           "Kotlin: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Se você já se perguntou como saber qual será a data daqui a uma semana ou em que dia da semana você nasceu, calcular datas no futuro ou passado pode ser útil. Além disso, pode ser um desafio divertido para testar suas habilidades de programação.

## Como Fazer

Para calcular uma data no futuro ou passado com Kotlin, é preciso usar a classe `LocalDate` da biblioteca `java.time`. Primeiro, importe a biblioteca no início do seu código:

```Kotlin
import java.time.LocalDate
```

Em seguida, é necessário criar uma instância da classe `LocalDate` com uma data específica. Por exemplo, para representar o dia de hoje, você pode usar a função `now()`:

```Kotlin
val dataAtual = LocalDate.now()
```

Para calcular uma data no futuro, utilize o método `plus` passando como parâmetro a quantidade desejada de dias, semanas, meses ou anos:

```Kotlin
val dataNoFuturo = dataAtual.plusWeeks(2) // soma 2 semanas à data atual
```

Já para calcular uma data no passado, utilize o método `minus`:

```Kotlin
val dataNoPassado = dataAtual.minusMonths(6) // subtrai 6 meses da data atual
```

Por fim, você pode imprimir a data resultante utilizando o método `format` e especificando o formato desejado:

```Kotlin
println("Data no futuro: ${dataNoFuturo.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))}") // imprime no formato dd/MM/yyyy
```

## Mergulho Profundo

A classe `LocalDate` possui diversos outros métodos úteis para manipulação de datas, como `plusDays`, `plusMonths`, `plusYears`, `minusDays`, `minusWeeks` e `minusMonths`. Além disso, também é possível realizar comparações entre datas utilizando os métodos `isBefore`, `isAfter` e `isEqual`.

Caso precise de mais precisão em relação a horários e fusos horários, a biblioteca `java.time` também oferece outras classes, como `LocalDateTime`, `ZonedDateTime` e `OffsetDateTime`.

## Veja Também

- [Documentação oficial da classe LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutorial sobre a biblioteca java.time](https://www.baeldung.com/kotlin/java-time-dates)
- [Outros recursos de Kotlin](https://kotlinlang.org/docs/reference/)