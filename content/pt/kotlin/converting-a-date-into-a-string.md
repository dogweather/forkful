---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Convertendo uma data em uma string em Kotlin

## O que é e por quê?

Converter uma data em uma string é a prática de formatar e apresentar datas de uma maneira legível para humanos. Os programadores fazem isso para que as datas possam ser facilmente interpretadas e manipuladas no código.

## Como fazer:

```Kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val agora = LocalDateTime.now()

    val formatoDeData = DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss")

    val stringData = agora.format(formatoDeData)

    println("Data em Formato String : $stringData")
}
```

Este bloco de código Kotlin acima irá produzir um resultado semelhante ao seguinte:

```Kotlin
Data em Formato String : 07-08-2021 23:45:12
```

## Mergulho profundo

Converter datas em strings é uma prática antiga na programação. LocalDateTime, usado acima, é uma classe Kotlin que simplifica muito a manipulação de datas.

Há várias alternativas para converter uma data em uma string em Kotlin. Além de `LocalDateTime`, você pode usar `SimpleDateFormat`, uma classe mais antiga que também permite formatar datas.

Quanto aos detalhes de implementação, o `DateTimeFormatter` inclui diferentes padrões para formatar datas. Você pode escolher um que melhor se adapte às suas necessidades!

## Veja também

"Documentação oficial do Kotlin para DateTimeFormatter": [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.jvm/-java-time/format.date-time/-java-time-format.date-time-fun-.html](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.jvm/-java-time/format.date-time/-java-time-format.date-time-fun-.html),

"Documentação oficial Kotlin para LocalDateTime": [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-date/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-date/)

"Postagem de blog sobre formatação de data e hora em Kotlin": [https://www.baeldung.com/kotlin/datetime-format](https://www.baeldung.com/kotlin/datetime-format)