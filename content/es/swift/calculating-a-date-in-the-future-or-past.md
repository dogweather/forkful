---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Swift: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Calcular una fecha en el futuro o pasado es una tarea común que los programadores realizan en sus proyectos. Consiste en determinar una fecha específica a partir de una fecha dada y una cantidad de tiempo (días, semanas, meses, etc.) hacia adelante o hacia atrás. Los programadores suelen realizar esta tarea para automatizar tareas relacionadas con el manejo de fechas, como programar recordatorios, calcular fechas de vencimiento o generar calendarios.

## Cómo:

```Swift
let today = Date() // Crear una variable con la fecha actual
let futureDate = Calendar.current.date(byAdding: .day, value: 7, to: today) // Calcular la fecha 7 días en el futuro
let pastDate = Calendar.current.date(byAdding: .month, value: -1, to: today) // Calcular la fecha 1 mes en el pasado

print("La fecha de hoy es: \(today)") // Imprimir la fecha actual
print("La fecha en 7 días será: \(futureDate!)") // Imprimir la fecha en 7 días
print("La fecha hace un mes fue: \(pastDate!)") // Imprimir la fecha hace un mes
```

La salida de este código sería algo como:

```
La fecha de hoy es: 2020-01-10 05:00:00 +0000
La fecha en 7 días será: 2020-01-17 05:00:00 +0000
La fecha hace un mes fue: 2019-12-10 05:00:00 +0000
```

## Profundizando:

Calcular fechas en el futuro o pasado es una tarea que se ha vuelto mucho más sencilla con el uso de lenguajes de programación modernos como Swift. Anteriormente, los programadores tenían que realizar cálculos más complejos para lograr el mismo resultado. Sin embargo, todavía existen otras formas de lograr esta tarea, como el uso de librerías externas o la implementación de algoritmos propios.

El cálculo de fechas en el futuro o pasado también es importante en el desarrollo de aplicaciones que requieren recordatorios o eventos programados. Al automatizar esta tarea, los programadores pueden ahorrar tiempo y esfuerzo en la gestión de fechas y asegurar una mayor precisión en la planificación.

## Ver también:

Si quieres saber más sobre cómo trabajar con fechas en Swift, puedes consultar la documentación oficial de Apple sobre fechas [aquí](https://developer.apple.com/documentation/swift/date). También puedes explorar opciones de librerías externas como [SwiftDate](https://github.com/malcommac/SwiftDate) o [DateTools](https://github.com/MatthewYork/DateTools).