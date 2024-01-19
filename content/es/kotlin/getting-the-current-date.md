---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Obtener la fecha actual significa programar tu aplicación para que, automáticamente, reconozca la fecha y hora del sistema. Esto es útil para eventos de registro, marcar la hora exacta de un incidente o incluso para funciones de tiempo real.

## Cómo hacerlo:
Usamos la biblioteca `java.util.Calendar` para esto. Aquí está el código en Kotlin.

```Kotlin
import java.util.Calendar

fun main(args: Array<String>) {
    val fechaActual = Calendar.getInstance().time

    println("La fecha actual es: $fechaActual")
}
```

Si corres este código, la salida será algo como esto:

```
La fecha actual es: Tue Sep 29 14:46:01 UTC 2020
```

## Más Detalles:
Históricamente, la forma en que los lenguajes de programación manejan las fechas y las horas ha variado. En Java, la biblioteca `java.util.Date` se usaba primariamente, pero tenía limitaciones significativas. Kotlin, al ser totalmente interoperable con Java, también puede utilizar `java.time`, la API moderna introducida en Java 8.

Como alternativa, puedes utilizar `LocalDateTime` de `java.time`. Aquí está un ejemplo de cómo hacerlo:

```Kotlin
import java.time.LocalDateTime

fun main(args: Array<String>) {
    val fechaActual = LocalDateTime.now()

    println("La fecha actual es: $fechaActual")
}
```

Este código te dará un resultado similar a nuestro ejemplo anterior. La elección entre `Calendar` y `LocalDateTime` depende en gran medida de tu caso de uso particular y del nivel de detalle que necesites.

## Ver También:
Para más detalles sobre el manejo de fechas y horas en Kotlin, puedes visitar los siguientes recursos:

1. Documentación oficial de Kotlin: [Java Interop](https://kotlinlang.org/docs/java-interop.html)
2. Guía de Kotlin de Baeldung: [LocalDateTime with Kotlin](https://www.baeldung.com/kotlin/localdatetime)
3. Documentación de Kotlin de Medium: [Working with Date and Time in Kotlin](https://medium.com/@kashifoza/working-with-date-and-time-in-kotlin-46f4a06675b1)

Recuerda, la clave para ser un buen programador es mantener la curiosidad y estar dispuesto a aprender constantemente. ¡Feliz programación!