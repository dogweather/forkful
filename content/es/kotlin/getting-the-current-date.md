---
title:                "Kotlin: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por Qué

En la programación, a menudo necesitamos obtener la fecha y hora actual en nuestro código para realizar ciertas tareas y funciones. Puede ser para registrar la fecha de un evento, programar acciones en un momento específico, o simplemente para mostrar la fecha actual en una interfaz de usuario. Afortunadamente, en Kotlin hay una manera sencilla de obtener la fecha actual en nuestro código. En esta publicación, exploraremos cómo hacerlo.

## Cómo Obtener la Fecha Actual en Kotlin

Para obtener la fecha actual en Kotlin, podemos usar la clase `java.time.LocalDate` incluida en la API de Java. Esta clase nos permite crear una instancia de fecha actual utilizando una de sus funciones estáticas `now()`. Luego, podemos obtener el día, mes y año actual de esta instancia y utilizarlos en nuestro código. Veamos un ejemplo de cómo hacerlo:

```Kotlin
//Importamos la clase LocalDate de la API de Java
import java.time.LocalDate

//Creamos una instancia de fecha actual
val fechaActual = LocalDate.now()

//Obtenemos el día, mes y año actual de la instancia
val diaActual = fechaActual.dayOfMonth
val mesActual = fechaActual.monthValue
val añoActual = fechaActual.year

//Mostramos la fecha actual en consola
println("Hoy es $diaActual/$mesActual/$añoActual") 
```

Si ejecutamos este código, obtendremos el siguiente resultado:

```
Hoy es 21/09/2021
```

También podemos obtener la hora actual utilizando la clase `java.time.LocalTime` de la misma manera. Aquí hay un ejemplo de cómo hacerlo:

```Kotlin
//Importamos la clase LocalTime de la API de Java
import java.time.LocalTime

//Creamos una instancia de hora actual
val horaActual = LocalTime.now()

//Obtenemos la hora, minuto y segundo actual de la instancia
val horaActual = horaActual.hour
val minutoActual = horaActual.minute
val segundoActual = horaActual.second

//Mostramos la hora actual en consola
println("La hora actual es $horaActual:$minutoActual:$segundoActual") 
```

El resultado de este código será similar a:

```
La hora actual es 15:30:50
```

## Profundizando en la Obtención de la Fecha Actual

La clase `java.time.LocalDate` tiene muchas más funciones útiles que podemos utilizar para obtener información detallada sobre la fecha actual, como el día de la semana, el número de semana del año, entre otras. También podemos utilizar diferentes formatos de fecha y hora, como `DateTimeFormatter`, para mostrar la fecha actual de diferentes maneras. Para obtener más información sobre todas las posibilidades que ofrece la clase `java.time.LocalDate`, siempre se puede consultar la documentación en línea.

## Ver También

- [Documentación de la clase LocalDate] (https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Documentación de la clase LocalTime] (https://docs.oracle.com/javase/8/docs/api/java/time/LocalTime.html)
- [Guía oficial de Kotlin sobre fechas y horas] (https://kotlinlang.org/docs/datetime/)