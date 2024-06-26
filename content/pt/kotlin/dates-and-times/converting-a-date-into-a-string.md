---
date: 2024-01-20 17:37:03.133034-07:00
description: "Como fazer: Historicamente, a manipula\xE7\xE3o de datas no Java, herdada\
  \ pelo Kotlin, era feita com classes como `java.util.Date` e\u2026"
lastmod: '2024-04-05T21:53:46.895133-06:00'
model: gpt-4-1106-preview
summary: "Historicamente, a manipula\xE7\xE3o de datas no Java, herdada pelo Kotlin,\
  \ era feita com classes como `java.util.Date` e `java.text.SimpleDateFormat`."
title: Convertendo uma data em uma string
weight: 28
---

## Como fazer:
```kotlin
import java.text.SimpleDateFormat
import java.util.Date

fun main() {
    val agora = Date()
    val formato = SimpleDateFormat("dd/MM/yyyy HH:mm:ss")
    val dataComoString = formato.format(agora)
    println(dataComoString) // Exemplo de saída: 30/03/2023 16:45:12
}
```

## Mergulho Profundo
Historicamente, a manipulação de datas no Java, herdada pelo Kotlin, era feita com classes como `java.util.Date` e `java.text.SimpleDateFormat`. Com a introdução do Java 8, surgiram as classes `java.time`, proporcionando uma API mais robusta e com melhor tratamento de zonas horárias. No Kotlin, além das APIs do Java, existem alternativas de uso mais idiomatico como o Kotlinx-datetime para lidar com datas e horas. 

A escolha entre usar o antigo `SimpleDateFormat` ou a nova `java.time.format.DateTimeFormatter` depende do contexto e da mínima versão da JVM que você planeja suportar. Em aplicações Android, por exemplo, `java.time` é seguro de usar a partir do Android API level 26 ou superior, a menos que você utilize o desugaring das Java Time APIs oferecidas pelo Android Studio.

Implementação com `DateTimeFormatter`:
```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val agora = LocalDateTime.now()
    val formato = DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm:ss")
    val dataComoString = agora.format(formato)
    println(dataComoString) // Exemplo de saída: 30/03/2023 16:45:12
}
```

## Veja Também
- Android Desugaring of Java Time: [Use java.time on older Android devices](https://developer.android.com/studio/write/java8-support#library-desugaring)
- Biblioteca Kotlinx-datetime: [Kotlinx-datetime GitHub](https://github.com/Kotlin/kotlinx-datetime)
