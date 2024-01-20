---
title:                "Análisis de una fecha a partir de una cadena"
date:                  2024-01-20T15:37:10.651064-07:00
html_title:           "Arduino: Análisis de una fecha a partir de una cadena"
simple_title:         "Análisis de una fecha a partir de una cadena"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Parsear una fecha significa convertir un texto a un tipo de dato de fecha. Los programadores lo hacen para poder manipular y comparar fechas más fácilmente en sus aplicaciones.

## Cómo:
```kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val dateString = "2023-04-05"
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val fecha = LocalDate.parse(dateString, formatter)
    
    println(fecha) // Output: 2023-04-05
}
```

## Inmersión Profunda
Parsear fechas es esencial ya que las fechas formateadas como texto no se pueden utilizar para cálculos o comparaciones. Históricamente, Java usaba `SimpleDateFormat`, pero a partir de Java 8 y Kotlin, `DateTimeFormatter` es preferible por su inmutabilidad y seguridad en entornos de hilos múltiples.

Alternativas incluyen bibliotecas como Joda-Time (ahora en desuso) o APIs de terceros. En Kotlin, también puedes usar extensiones para agregar funcionalidades de parseo directamente a las clases de String.

La implementación de `DateTimeFormatter` aprovecha el patrón de diseño de 'Builder' para su creación, haciendo que configurar formatos complejos sea sencillo y expresivo.

## Ver También
- [Kotlin Documentation - Basic Types](https://kotlinlang.org/docs/basic-types.html)
- [Oracle JavaDocs - DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Baeldung - A Guide to DateTimeFormatter](https://www.baeldung.com/java-datetimeformatter)