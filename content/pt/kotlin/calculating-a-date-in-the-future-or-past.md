---
title:                "Calculando uma data no futuro ou passado"
date:                  2024-01-20T17:31:31.537423-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Calcular datas no futuro ou no passado é basicamente adicionar ou subtrair dias, meses ou anos a uma data específica. Programadores fazem isso para gerenciar prazos, calcular idades ou definir cronogramas em aplicativos.

## Como fazer:
```kotlin
import java.time.LocalDate
import java.time.temporal.ChronoUnit

fun main() {
    val hoje = LocalDate.now()
    val daquiAUmMes = hoje.plusMonths(1)
    val haCincoDias = hoje.minusDays(5)

    println("Hoje: $hoje")
    println("Daqui a um mês: $daquiAUmMes")
    println("Há cinco dias: $haCincoDias")
}
```
Saída de exemplo:
```
Hoje: 2023-04-12
Daqui a um mês: 2023-05-12
Há cinco dias: 2023-04-07
```

## Aprofundando
Antes de termos bibliotecas modernas como `java.time` (introduzida no Java 8), os programadores muitas vezes se batiam com as inconveniências da classe `Date` e `Calendar`. A precisão e facilidades de manipulação de datas eram limitadas e propensas a erros. Agora, com essas novas classes, ajustar datas tornou-se muito mais intuitivo e seguro contra erros comuns.

Alternativas existem, como Joda-Time, que já oferecia muitas dessas funcionalidades antes do Java 8. Contudo, com a chegada do `java.time`, Joda-Time recomendou que novos projetos utilizassem as classes da biblioteca padrão.

Um detalhe da implementação no cálculo de datas é lidar com questões como anos bissextos e zonas horárias. O `java.time` lida com isso internamente, simplificando o trabalho do programador.

## Veja também
- Documentação oficial do `java.time` para maiores detalhes: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- "Kotlin in Action": um livro com um capítulo dedicado ao tratamento de datas e horas.
- Kotlin Playground para testar os códigos: [https://play.kotlinlang.org](https://play.kotlinlang.org)
