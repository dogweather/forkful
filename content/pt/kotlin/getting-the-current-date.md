---
title:    "Kotlin: Obtendo a data atual"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Por que obter a data atual?

Obter a data atual é uma tarefa comum em muitos programas Kotlin. Isso é útil para registrar eventos, rastrear o tempo de execução e realizar cálculos de tempo. Neste artigo, vamos dar uma olhada em como obter a data atual em Kotlin e também entender alguns conceitos mais aprofundados sobre o assunto.

## Como obter a data atual em Kotlin

Em Kotlin, a classe `LocalDate` do pacote `java.time` nos fornece métodos para obter a data atual e manipulá-la conforme desejado.

```Kotlin
import java.time.LocalDate

fun main() {
    // Obter a data atual
    val currentDate = LocalDate.now()
    println(currentDate)

    // Obter o dia atual
    val day = currentDate.dayOfMonth
    println("Dia atual: $day")

    // Obter o mês atual
    val month = currentDate.month
    println("Mês atual: $month")

    // Obter o ano atual
    val year = currentDate.year
    println("Ano atual: $year")
}
```

A saída do código acima será:

```
2021-10-14
Dia atual: 14
Mês atual: OCTOBER
Ano atual: 2021
```

## Deep Dive: Mais detalhes sobre a data atual

A classe `LocalDate` possui vários outros métodos úteis para manipular datas. Vejamos alguns exemplos:

- Alterar a data para o primeiro dia do mês atual:
```Kotlin
val firstDayOfMonth = currentDate.withDayOfMonth(1)
```

- Adicionar 5 dias à data atual:
```Kotlin
val newDate = currentDate.plusDays(5)
```

- Comparar duas datas:
```Kotlin
val date1 = LocalDate.of(2021, 10, 14)
val date2 = LocalDate.of(2020, 5, 25)
println(date1.isAfter(date2)) // true
```

Além disso, a classe `LocalDate` também permite a conversão entre diferentes formatos de data, como `java.util.Date` e `java.sql.Date`.

# Veja também

- [Documentação oficial do Kotlin sobre trabalhar com datas](https://kotlinlang.org/docs/datetime.html)
- [Tutorial do Kotlin sobre a classe LocalDate](https://www.tutorialkart.com/kotlin/localdate/)
- [Guia completo de formatação de datas em Kotlin](https://www.baeldung.com/kotlin-format-datetime)