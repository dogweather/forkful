---
title:                "Comparando dos fechas"
html_title:           "C#: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Comparación de Fechas en Kotlin: Una Guía Simplicada 

## ¿Qué y por qué?

Comparar dos fechas nos ayuda a determinar cuál es anterior o posterior. Los programadores lo hacen para organizar eventos, establecer límites de tiempo o incluso medir el tiempo de ejecución de un código.

## Cómo hacerlo:

En Kotlin, puedes usar el operador `compareTo` para comparar dos fechas. Mira este ejemplo sencillo.

```Kotlin
import java.util.Date

fun main() {
    val fecha1 = Date()
    Thread.sleep(2000)
    val fecha2 = Date()

    when {
        fecha1 < fecha2 -> println("La fecha1 es anterior a la fecha2")
        fecha1 > fecha2 -> println("La fecha1 es posterior a la fecha2")
        else -> println("Las fechas son iguales.")
    }
}
```
Este código devuelve: "La fecha1 es anterior a la fecha2" porque en este caso, esperamos 2 segundos antes de crear `fecha2`.

## Tras la Cámara

Históricamente, la comparación de fechas procede del deseo de entender el tiempo más allá de ahora. Las fechas permiten a los informáticos rastrear el tiempo entre eventos y ordenarlos secuencialmente.

El método `compareTo` en Kotlin es una implementación directa de la interfaz `Comparable`. Ofrece una forma fácil y eficiente de comparar dos fechas. No obstante, para situaciones más complejas, podemos usar las clases LocalDateTime, LocalDate y LocalTime junto con la librería de tiempo Joda.

Con `compareTo`, tanto la fecha como la hora se consideran en la comparación. Si solo quieres considerar la fecha y no la hora, es mejor usar LocalDate.

## Ver también

Aquí hay algunos recursos útiles para profundizar en el manejo y la comparación de tiempo en Kotlin:

1. Comparación de fechas en Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/
2. Librería de tiempo Joda: http://www.joda.org/joda-time/
3. Uso de LocalDate, LocalDateTime y LocalTime: https://www.baeldung.com/kotlin/dates