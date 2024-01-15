---
title:                "Obteniendo la fecha actual"
html_title:           "Elm: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por Qué
Si alguna vez has necesitado obtener la fecha actual en un programa en Elm, es muy probable que hayas buscado en internet cómo hacerlo. Obtener la fecha actual es una tarea común en la programación y en este artículo te enseñaremos cómo hacerlo de manera sencilla y eficiente.

## ¿Cómo Hacerlo?
```Elm
import Time

currentDate : Time.Posix
currentDate =
    Time.now
```

Con solo importar el módulo Time, podemos acceder a la función `now` que nos devuelve la fecha y hora actuales en formato `Time.Posix`. Esta función utiliza el sistema de reloj interno de la máquina para obtener la fecha actual. Podemos convertir este formato a una representación más legible utilizando otras funciones del módulo Time, como `toDate` o `toTime`.

```Elm
import Time exposing (Date, Time)
import Time.Format as Format

currentDate : Date
currentDate =
    Time.now
        |> Time.toTime -- Convertimos a Time
        |> Format.date Format.short -- Convertimos a formato de fecha

```

En el ejemplo anterior, utilizamos las funciones `toTime` y `Format.date` para convertir la fecha actual a un formato corto legible, como por ejemplo "11/05/2021".

## Deep Dive
Obtener la fecha actual puede parecer una tarea sencilla, pero en realidad hay mucho más detrás de ello. Como mencionamos anteriormente, la función `now` utiliza el sistema de reloj interno de la máquina, pero ¿cómo funciona esto?

El sistema de reloj interno es una parte importante del sistema operativo que se encarga de hacer un seguimiento del tiempo transcurrido desde que se inició la máquina. Esta información es utilizada por el sistema para muchas tareas, como por ejemplo la gestión de procesos o la sincronización de tareas.

Además, la representación de la fecha y hora en formato `Time.Posix` es una convención común en programación, ya que nos permite manipular estos valores de manera sencilla y realizar diversas operaciones matemáticas con ellos.

## Ver También
- [Documentación oficial de Elm sobre el módulo Time](https://package.elm-lang.org/packages/elm/time/latest/)
- [Artículo sobre cómo trabajar con fechas en Elm](https://dev.to/tmattio/manipulate-date-and-time-in-elm-3epf)
- [Explicación detallada de cómo funciona el sistema de reloj interno en los sistemas operativos](https://medium.com/@kpc700/como-funciona-el-reloj-interno-de-un-sistema-operativo-7b0994e95b66)