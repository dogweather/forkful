---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:15:35.201176-07:00
simple_title:         "Obtendo a data atual"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Pegar a data atual significa capturar o momento presente no nosso código. Programadores fazem isso para logs, funcionalidades de tempo real e tudo que precisa de uma marca temporal.

## Como fazer:

A forma mais direta no Kotlin é usar a biblioteca `java.time.LocalDate` para o dia atual:

```kotlin
import java.time.LocalDate

fun main() {
    val hoje = LocalDate.now()
    println(hoje)
}
```

Saída de exemplo:

```
2023-03-15
```

Se você precisar de mais detalhes como hora, minuto e segundo, use `java.time.LocalDateTime`:

```kotlin
import java.time.LocalDateTime

fun main() {
    val agora = LocalDateTime.now()
    println(agora)
}
```

Saída de exemplo:

```
2023-03-15T12:30:45.123
```

## Deep Dive

A necessidade de gerenciar datas e horas no software é tão antiga quanto a própria programação. No Kotlin, estamos frequentemente trabalhando em cima da API `java.time`, introduzida no Java 8, que substituiu as antigas `java.util.Date` e `java.util.Calendar` por ser mais segura quanto a threads e mais intuitiva.

Quanto às alternativas, além da API `java.time`, você pode usar bibliotecas de terceiros como Joda-Time, que era a escolha padrão antes do Java 8. Contudo, a partir do momento que a `java.time` se tornou a escolha moderna, ela se tornou a maneira mais recomendada para se trabalhar com datas e horas em Kotlin.

A implementação para pegar a data e a hora atuais usa o relógio do sistema padrão, que pode ser alterado para um relógio diferente se necessário, o que é útil para testes ou para lidar com fusos horários e localidades específicos.

## Veja Também

- Documentação da API `java.time`: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Kotlin API reference: https://kotlinlang.org/api/latest/jvm/stdlib/
- Tutorial do Joda-Time: https://www.joda.org/joda-time/quickstart.html
- Informações sobre fusos horários em Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-date/get-timezone-offset.html
