---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Kotlin: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Calcular una fecha en el futuro o pasado es un proceso para proyectar con precisión los puntos en el tiempo. Los programadores a menudo realizan cálculos de fecha para mantener seguimiento de los eventos, programar tareas y establecer tiempos de espera.

## ¿Cómo Hacerlo?

Aquí hay un ejemplo de cómo calcular una fecha en el futuro utilizando Kotlin. Supongamos que queremos saber la fecha dentro de 10 días.

``` Kotlin
import java.time.LocalDate
fun main() {
    val fechaActual = LocalDate.now()
    val fechaFutura = fechaActual.plusDays(10)
    println("Fecha futura: $fechaFutura")
}
```

Si corres este código, obtendrás un resultado como esto:

``` 
Fecha futura: 2023-06-10
```

Para una fecha en el pasado, solo necesitamos alterar una línea. Por ejemplo, si queremos saber la fecha hace 10 días:

``` Kotlin
import java.time.LocalDate
fun main() {
    val fechaActual = LocalDate.now()
    val fechaPasada = fechaActual.minusDays(10)
    println("Fecha pasada: $fechaPasada")
}
```

Corriendo este código, obtendrías algo así:

```
Fecha pasada: 2023-05-21
```

## Análisis Profundo

Historicamente, los cálculos de fechas se realizaron manualmente, siendo una tarea ardua y propensa a errores. Kotlin y otros lenguajes modernos han simplificado esta tarea proporcionando bibliotecas para manipulación de fechas.

En Kotlin, tienes varias alternativas para manejar las fechas. Puedes usar `Calendar` o `Date` además de `LocalDate`, pero `LocalDate` es más recomendado ya que es inmutable y tiene una API más intuitiva.

Los cálculos de fecha en Kotlin, especialmente la adición de días con 'plusDays' y sustracción con 'minusDays', se manejan automáticamente teniendo en cuenta los años bisiestos y otros factores del calendario, lo que hace que la implementación sea bastante robusta.

## Ver También

Para un conocimiento más profundo y avanzado, puedes visitar los siguientes enlaces:

- Documentación oficial de Kotlin sobre la fecha y hora: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-date/
- Guía de Java/Kotlin de baeldung sobre cómo calcular la diferencia entre dos fechas: https://www.baeldung.com/kotlin/dates
- Artículo "Working with Date, Time, and ChronoUnit Enum in Kotlin" de Medium: https://medium.com/@igorkulikov/working-with-date-time-and-chronounit-enum-in-kotlin-b1e5fb1d48ee