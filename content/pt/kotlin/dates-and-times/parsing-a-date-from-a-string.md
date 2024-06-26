---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:30.098795-07:00
description: "Como Fazer: Kotlin suporta a an\xE1lise de datas atrav\xE9s do pacote\
  \ `java.time`, introduzido no Java 8. Aqui est\xE1 uma abordagem simples usando\
  \ `LocalDateTime`\u2026"
lastmod: '2024-03-13T22:44:46.551644-06:00'
model: gpt-4-0125-preview
summary: "Kotlin suporta a an\xE1lise de datas atrav\xE9s do pacote `java.time`, introduzido\
  \ no Java 8."
title: Analisando uma data a partir de uma string
weight: 30
---

## Como Fazer:
Kotlin suporta a análise de datas através do pacote `java.time`, introduzido no Java 8. Aqui está uma abordagem simples usando `LocalDateTime` e um padrão específico:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun parseDateFromString(dateString: String): LocalDateTime {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    return LocalDateTime.parse(dateString, formatter)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateFromString(dateString)
    println(date)  // Saída: 2023-04-01T12:00
}
```

Para mais flexibilidade, ou para lidar com datas de fontes externas como APIs, você poderia usar uma biblioteca de terceiros, como Joda-Time (embora seja menos comum agora com `java.time` sendo robusto). No entanto, para a maioria das aplicações Kotlin, é preferível seguir a abordagem moderna fornecida pelo JDK.

Para analisar uma data em Kotlin sem usar bibliotecas de terceiros, você também pode utilizar a classe `SimpleDateFormat` para versões anteriores ao Java 8 ou níveis de API do Android que não suportam `java.time`:

```kotlin
import java.text.SimpleDateFormat

fun parseDateUsingSimpleDateFormat(dateString: String): java.util.Date {
    val formatter = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    return formatter.parse(dateString)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateUsingSimpleDateFormat(dateString)
    println(date)  // A saída variará com base no seu fuso horário, por exemplo, Sat Apr 01 12:00:00 GMT 2023
}
```

Lembre-se de sempre definir o fuso horário ao trabalhar com `SimpleDateFormat` para evitar deslocamentos inesperados nas datas analisadas.
