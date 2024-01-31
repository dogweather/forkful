---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:15:56.095382-07:00
html_title:           "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Obtener la fecha actual en programación es capturar el momento exacto en que se ejecuta una línea de código. Los desarrolladores lo hacen para registrar eventos, validar periodos de tiempo, y personalizar funciones dependiendo de la fecha.

## Cómo hacerlo:

Kotlin hace que trabajar con fechas sea sencillo. Utiliza la librería `java.time.LocalDate` para conseguir la fecha de hoy:

```kotlin
import java.time.LocalDate

fun main() {
    val hoy = LocalDate.now()
    println("La fecha de hoy es: $hoy")
}
```

Para fechas y horas más específicas, emplea `java.time.LocalDateTime`:

```kotlin
import java.time.LocalDateTime

fun main() {
    val fechaHoraActual = LocalDateTime.now()
    println("La fecha y hora actuales son: $fechaHoraActual")
}
```

Ejecuta el código. La salida depende del día actual y...

Si es 12 de abril de 2023:

```
La fecha de hoy es: 2023-04-12
La fecha y hora actuales son: 2023-04-12T15:30:45.123
```

## Profundizando:

`java.time`, también conocida como JSR-310, es una moderna API de fecha y hora incorporada en Java 8 y disponible en Kotlin. Antes de `java.time`, gestionar fechas era más complejo y propenso a errores con `java.util.Date` y `java.util.Calendar`.

Las alternativas para trabajar con fechas en Kotlin incluyen la antigua `java.util` y librerías de terceros como Joda-Time. Sin embargo, `java.time` es más recomendada por su inmutabilidad y diseño con una conciencia de zona horaria. Además, Kotlin ofrece extensiones que mejoran la API como `kotlinx-datetime`.

Implementar fechas con `java.time` es directo gracias a métodos como `LocalDate.now()` o `LocalDateTime.now()`. Todas las clases en `java.time` son inmutables y thread-safe, lo que significa que son seguras para usar en aplicaciones concurrentes.

## Ver También:

Para expandir tu conocimiento en el manejo de fechas en Kotlin:

- Documentación oficial de Kotlin: [https://kotlinlang.org/docs/home.html](https://kotlinlang.org/docs/home.html)
- Guía de `java.time`: [https://docs.oracle.com/javase/tutorial/datetime/](https://docs.oracle.com/javase/tutorial/datetime/)
- `kotlinx-datetime` para Kotlin Multiplatform: [https://github.com/Kotlin/kotlinx-datetime](https://github.com/Kotlin/kotlinx-datetime)
