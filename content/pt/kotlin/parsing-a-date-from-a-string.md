---
title:                "Analisando uma data a partir de uma string"
date:                  2024-01-20T15:37:09.374885-07:00
html_title:           "Arduino: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Transformar strings em datas permite entender e manipular temporalmente a informação. No dia a dia do desenvolvimento, datas são frequentemente manipuladas para agendamentos, registros ou comparações.

## Como Fazer:
```kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.Locale

fun main() {
    val dataString = "15/04/2023"
    val formatador = DateTimeFormatter.ofPattern("dd/MM/yyyy").withLocale(Locale("pt", "BR"))
    val data = LocalDate.parse(dataString, formatador)
    
    println(data) // Saída: 2023-04-15
}
```

## Mergulho Profundo
Historicamente, manipulação de datas no Java era complexa e bug-prone, o que levou à criação da API `java.time` no Java 8, que o Kotlin adotou por ser mais segura e intuitiva. Alternativas incluem o uso das APIs antigas `java.util.Date` ou `java.util.Calendar`, mas a API `java.time` é a recomendada. As implementações específicas da localidade no parsing permitem que datas sejam interpretadas no contexto cultural adequado, um ponto importante em software globalizado.

## Veja Também
- [Documentação oficial da API java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Kotlin Playground para experimentar código](https://play.kotlinlang.org/)
- [Guia de padrões de formatação de data e hora](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)