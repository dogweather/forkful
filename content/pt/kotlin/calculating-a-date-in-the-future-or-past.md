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

# Trabalhando com Datas em Kotlin: Um Guia Rápido

## O que & Por quê?

Calcular uma data futura ou passada é a tarefa de adicionar ou subtrair uma quantidade específica de tempo a uma data inicial. Programadores o fazem para resolver problemas do mundo real como o cálculo de prazos, aniversários e muito mais.

## Como fazer:

No Kotlin, podemos utilizar a biblioteca java.time disponível a partir do JDK 8.

```kotlin
import java.time.LocalDate
import java.time.temporal.ChronoUnit

fun main() {
    val hoje = LocalDate.now()
    println("Hoje: $hoje")

    val umaSemanaDepois = hoje.plus(1, ChronoUnit.WEEKS)
    println("Uma semana depois: $umaSemanaDepois")

    val umMesAntes = hoje.minus(1, ChronoUnit.MONTHS)
    println("Um mês antes: $umMesAntes")
}
```
Na saída, você verá algo assim:
```output
Hoje: 2022-01-01
Uma semana depois: 2022-01-08
Um mês antes: 2021-12-01
```

## Deep Dive:

Historicamente, muitos programadores usaram as bibliotecas de datas e horas de Java, como java.util.Date e java.util.Calendar, mas essas classes são propensas a erros e menos intuitivas.

Uma alternativa é a biblioteca Joda-Time, que foi a base para a nova API java.time. No entanto, java.time é mais aprimorado e recomendado para uso atual.

Como estamos trabalhando com datas, temos que estar cientes dos detalhes de implementação como as variações do fuso horário e a mudança para o horário de verão.

## Veja Também:

1. [Documentação Oficial Java.time](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
2. [Joda-Time](https://www.joda.org/joda-time/)