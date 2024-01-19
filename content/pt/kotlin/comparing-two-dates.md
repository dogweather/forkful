---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Comparando duas datas em Kotlin

## O Que & Porquê?

Comparar duas datas significa verificar se uma data é anterior, posterior ou igual à outra. Os programadores fazem isso para distinguir entre datas, para ordenar eventos no tempo ou para fazer cálculos de tempo.

## Como fazer:

Em Kotlin, o processo de comparação de datas é bastante simples, graças à biblioteca padrão. Aqui estão alguns exemplos:

```Kotlin 
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2020, 7, 20)
    val date2 = LocalDate.of(2021, 8, 21)

    when {
        date1.isAfter(date2) -> println("A date1 é posterior à date2")
        date1.isBefore(date2) -> println("A date1 é anterior à date2")
        else -> println("As datas são iguais")
    }
}
``` 
Saída:

``` 
A date1 é anterior à date2
```

## Mergulho Profundo 

Historicamente, a manipulação de datas tem sido uma dor de cabeça para os programadores em várias linguagens. Felizmente, Kotlin torna o processo bastante simples e tiposafe com o uso de `LocalDate` e `LocalDateTime`.

Existem muitas alternativas disponíveis se você quiser mais versatilidade. Você pode usar bibliotecas externas, tais como Joda-Time ou ThreeTenABP, cada uma com suas próprias vantagens e desvantagens. Conhecer as bibliotecas padrão, no entanto, é sempre uma coisa boa.

## Veja Também:

- [Documentação oficial sobre LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-local-date/)
- [Comparing dates in Java](https://www.baeldung.com/java-date-comparison)
- [Joda-Time library](https://www.joda.org/joda-time/)
- [ThreeTenABP library](https://github.com/JakeWharton/ThreeTenABP)