---
title:                "Kotlin: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas?

Comparar datas é uma tarefa comum ao desenvolver um aplicativo ou programa que lida com informações temporais. Pode ser necessário verificar se uma data é anterior, posterior ou igual a outra, ou até mesmo calcular a diferença entre elas. Neste artigo, vamos explorar como comparar datas em Kotlin de forma eficiente e precisa.

## Como fazer

Para comparar datas em Kotlin, podemos utilizar as classes `LocalDate` e `LocalDateTime` da biblioteca padrão `java.time`. Primeiro, precisamos criar instâncias dessas classes com as datas que desejamos comparar:

```Kotlin
val data1 = LocalDate.of(2021, 5, 10)
val data2 = LocalDateTime.of(2021, 5, 12, 18, 30)
```

Em seguida, podemos utilizar os métodos `isBefore()`, `isAfter()` e `isEqual()` para verificar se uma data é anterior, posterior ou igual à outra, respectivamente.

```Kotlin
println(data1.isBefore(data2)) //true
println(data1.isAfter(data2)) //false
println(data1.isEqual(data2)) //false
```

Também podemos comparar as datas diretamente utilizando os operadores `==`, `<` e `>`. Por exemplo:

```Kotlin
println(data1 == data2) //false
println(data1 < data2) //true
println(data1 > data2) //false
```

Também é possível calcular a diferença entre as datas em dias, semanas, meses ou anos usando os métodos `until()` ou `between()`:

```Kotlin
val diferencaEmDias = data1.until(data2, ChronoUnit.DAYS)

println(diferencaEmDias) //2
```

## Profundidade

Ao trabalhar com datas em Kotlin, é importante estar ciente de alguns detalhes que podem afetar a precisão das comparações. Por exemplo, a classe `LocalDate` não leva em consideração o fuso horário, enquanto a classe `LocalDateTime` sim. Isso pode resultar em resultados inesperados ao comparar datas com diferentes fusos horários.

Também é importante lembrar que as datas são imutáveis em Kotlin, portanto, ao realizar operações de comparação ou cálculo de diferença, uma nova instância da data é criada.

## Veja também

- [Documentação oficial do Kotlin - Comparing Dates](https://kotlinlang.org/docs/datetime.html#comparing-dates)
- [Tutorialspoint - Kotlin Date and Time](https://www.tutorialspoint.com/kotlin/kotlin_date_time.htm)
- [Baeldung - Comparing Dates in Java and Kotlin](https://www.baeldung.com/java-compare-dates)