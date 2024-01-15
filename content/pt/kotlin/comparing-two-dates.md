---
title:                "Comparando duas datas"
html_title:           "Kotlin: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas

Comparar duas datas pode ser útil em diferentes situações, como em aplicações de planejamento, gerenciamento de eventos, ou até mesmo em jogos que envolvem tempo. Ao comparar duas datas, você pode determinar qual é a mais recente, se são iguais, ou calcular a diferença entre elas.

## Como fazer em Kotlin

Para comparar duas datas em Kotlin, você pode utilizar a classe `LocalDate` do pacote `java.time`. Primeiro, importe o pacote:

```Kotlin
import java.time.LocalDate
```

Em seguida, crie duas variáveis do tipo `LocalDate` para armazenar suas datas:

```Kotlin
val data1 = LocalDate.of(2021, 5, 15)
val data2 = LocalDate.of(2021, 7, 10)
```

Agora, para comparar as duas datas, podemos usar os operadores de comparação `<`, `>`, `<=` e `>=`:

```Kotlin
println(data1 < data2) //output: true
println(data1 > data2) //output: false
println(data1 <= data2) //output: true
println(data1 >= data2) //output: false
```

Você também pode usar o método `isEqual()` para verificar se as datas são iguais:

```Kotlin
println(data1.isEqual(data2)) //output: false
```

Além disso, é possível calcular a diferença entre as datas usando o método `until()` e especificando a unidade de tempo:

```Kotlin
println(data1.until(data2).days) //output: 56
println(data1.until(data2).months) //output: 1
```

## Mergulho profundo

Ao comparar duas datas, é importante levar em consideração alguns fatores como o fuso horário e a necessidade de converter as datas para um formato válido antes da comparação. Também é possível comparar datas com horários, utilizando a classe `LocalDateTime` do pacote `java.time`.

## Veja também

- [Documentação oficial do Kotlin sobre a classe LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date/) 
- [Java Time API Overview and Examples](https://www.baeldung.com/java-time)