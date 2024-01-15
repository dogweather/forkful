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

## ¿Por qué?

Calcular fechas en el futuro o en el pasado es una tarea común en la programación. Ya sea para planificar eventos o para realizar cálculos de tiempo, esta habilidad es esencial para cualquier programador.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado en Kotlin, necesitaremos utilizar la clase `LocalDate` de la librería `java.time`. Esta clase nos permite trabajar con fechas de una manera sencilla y eficiente.

Para obtener la fecha actual, podemos utilizar el método `now()` y luego utilizar el método `plus()` o `minus()` para añadir o restar días, meses o años a la fecha actual. Veamos un ejemplo de cómo calcular la fecha de hoy más un año:

```Kotlin
val fechaActual = LocalDate.now()
val fechaEnElFuturo = fechaActual.plusYears(1)
println(fechaEnElFuturo) // Output: 2022-02-06
```

En este ejemplo, estamos utilizando el método `plusYears()` para sumar un año a la fecha actual y luego imprimimos la nueva fecha.

Además de sumar y restar años, también podemos utilizar métodos como `plusDays()` o `plusMonths()` para realizar cálculos con días o meses.

Si queremos calcular una fecha en el pasado, podemos utilizar el método `minus()` de la misma manera. Por ejemplo, si queremos obtener la fecha de hace 6 meses, podemos hacerlo de la siguiente manera:

```Kotlin
val fechaActual = LocalDate.now()
val fechaEnElPasado = fechaActual.minusMonths(6)
println(fechaEnElPasado) // Output: 2021-08-06
```

## Profundizando

La clase `LocalDate` también nos permite trabajar con diferentes zonas horarias (timezones) y ajustarlas según sea necesario. Podemos utilizar el método `atZone()` para especificar una zona horaria y utilizar métodos como `withZoneSameInstant()` para convertir la fecha a otra zona horaria.

También podemos utilizar la clase `LocalDateTime` para trabajar con fechas y horas juntas y realizar cálculos más complejos.

Si quieres aprender más sobre cómo trabajar con fechas en Kotlin, te recomendamos echar un vistazo a la documentación oficial de `java.time` y explorar todas las posibilidades que ofrece esta librería.

## Vea también

- Documentación oficial de `java.time` en Kotlin (https://kotlinlang.org/docs/tutorials/datetime)
- Tutorial sobre cómo calcular fechas en Kotlin (https://levelup.gitconnected.com/calculate-dates-in-kotlin-86120e2b0dc2)
- Ejemplos de código en Kotlin para trabajar con fechas y horas (https://github.com/Kotlin/kotlin-examples/tree/master/stdlib/datetime)