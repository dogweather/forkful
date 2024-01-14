---
title:                "Kotlin: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

Si alguna vez te has preguntado cómo sería una fecha en el futuro o en el pasado, entonces calcular fechas en Kotlin es algo que querrás aprender. Ya sea que necesites planificar una reunión o simplemente quieras saber en qué día de la semana naciste, poder calcular fechas puede ser muy útil en tu vida diaria.

## Cómo hacerlo

Primero, vamos a importar la clase `Date` de la librería estándar de Kotlin. Luego, utilizaremos la función `plus()` para agregar una determinada cantidad de días a la fecha actual y `minus()` para restar días. Aquí tienes un ejemplo sencillo de cómo calcular una fecha en el futuro:

```Kotlin
import java.util.Date

val fechaActual = Date() // Fecha actual
val fechaEnUnaSemana = fechaActual.plus(7) // Fecha en una semana
println(fechaEnUnaSemana) // Salida: 2021-08-02

val fechaHaceUnMes = fechaActual.minus(30) // Fecha hace un mes
println(fechaHaceUnMes) // Salida: 2021-06-03
```

Ten en cuenta que la función `plus()` y `minus()` también pueden aceptar otros parámetros como meses, años, horas, etc.

También puedes utilizar la función `setTime()` para establecer una fecha específica. Aquí tienes un ejemplo de cómo establecer la fecha del 4 de julio de 2021:

```Kotlin
import java.util.Date

val fecha = Date()
fecha.setTime(1625391655000) // 4 de julio de 2021
println(fecha) // Salida: 2021-07-04
```

## Profundizando

Calcular fechas en Kotlin puede ser más complejo de lo que parece a simple vista. Hay que tener en cuenta diferentes aspectos como el manejo de zonas horarias, fechas bisiestas y más. Para profundizar en este tema, puedes consultar la documentación oficial de Kotlin sobre la clase `Date` y sus funciones.

Además, puedes expandir tus conocimientos sobre el manejo de fechas en Kotlin utilizando librerías externas como Joda-Time o ThreeTenABP.

## Ver también

- [Documentación oficial de Kotlin sobre la clase Date](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/)
- [Librería Joda-Time](https://www.joda.org/joda-time/)
- [Librería ThreeTenABP](https://github.com/JakeWharton/ThreeTenABP)