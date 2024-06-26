---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:23.623733-07:00
description: "C\xF3mo hacerlo: Kotlin soporta el an\xE1lisis de fechas a trav\xE9\
  s del paquete `java.time`, introducido en Java 8. Aqu\xED hay un enfoque simple\
  \ usando\u2026"
lastmod: '2024-03-13T22:44:59.045537-06:00'
model: gpt-4-0125-preview
summary: "Kotlin soporta el an\xE1lisis de fechas a trav\xE9s del paquete `java.time`,\
  \ introducido en Java 8."
title: Analizando una fecha a partir de una cadena de texto
weight: 30
---

## Cómo hacerlo:
Kotlin soporta el análisis de fechas a través del paquete `java.time`, introducido en Java 8. Aquí hay un enfoque simple usando `LocalDateTime` y un patrón específico:

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
    println(date)  // Salida: 2023-04-01T12:00
}
```

Para más flexibilidad, o para manejar fechas de fuentes externas como APIs, podrías usar una biblioteca de terceros como Joda-Time (aunque es menos común ahora con `java.time` siendo robusto). Sin embargo, ceñirse al enfoque moderno proporcionado por el JDK es preferido para la mayoría de las aplicaciones Kotlin.

Para analizar una fecha en Kotlin sin usar bibliotecas de terceros, también puedes hacer uso de la clase `SimpleDateFormat` para versiones antes de Java 8 o niveles de API de Android que carecen de soporte `java.time`:

```kotlin
import java.text.SimpleDateFormat

fun parseDateUsingSimpleDateFormat(dateString: String): java.util.Date {
    val formatter = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    return formatter.parse(dateString)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateUsingSimpleDateFormat(dateString)
    println(date)  // La salida variará según tu zona horaria, por ejemplo, Sat Apr 01 12:00:00 GMT 2023
}
```

Recuerda siempre establecer la zona horaria si trabajas con `SimpleDateFormat` para evitar desfases inesperados en las fechas analizadas.
