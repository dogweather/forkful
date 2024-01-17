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

## ¿Qué y por qué?
Calcular una fecha en el futuro o en el pasado es una tarea común para los programadores. Se refiere a encontrar una fecha específica, ya sea en el futuro o en el pasado, basado en una fecha de partida y una cantidad de tiempo (días, meses, años) especificada. Los programadores pueden utilizar esta función para realizar tareas como calcular plazos de entrega o programar recordatorios.

## Cómo:
Puedes calcular una fecha en el futuro o en el pasado en Kotlin utilizando la clase `LocalDate` y sus métodos `plus` y `minus`.

```Kotlin
// Calculando una fecha 3 meses en el futuro
val fechaActual = LocalDate.now()
val fechaFutura = fechaActual.plusMonths(3)
println("La fecha en 3 meses será: $fechaFutura") // Output: La fecha en 3 meses será: 2022-04-16

// Calculando una fecha 1 año en el pasado
val fechaPasada = fechaActual.minusYears(1)
println("La fecha hace 1 año fue: $fechaPasada") // Output: La fecha hace 1 año fue: 2020-04-16
```

## Profundizando:
La clase `LocalDate` fue introducida en Java 8 y fue diseñada para ser una alternativa más moderna a la clase `Date` con funcionalidades más completas y menos propensa a errores. Utilizar la clase en línea con los métodos de `plus` y `minus` para calcular fechas en el futuro o en el pasado es más sencillo y menos propenso a errores que realizar cálculos matemáticos con fechas.

Además, también existe la clase `LocalDateTime` que permite la manipulación de fechas y horas específicas.

## Ver también:
Para obtener más información sobre la clase `LocalDate` y sus métodos, puedes consultar la documentación oficial de Kotlin: https://kotlinlang.org/docs/reference/datetime.html#date-time-arithmetic