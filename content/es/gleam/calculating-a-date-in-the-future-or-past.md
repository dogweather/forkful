---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Gleam: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué & por qué?

Calcular una fecha en el futuro o en el pasado es un proceso común en la programación que nos permite manejar de manera eficiente y precisa el tiempo en nuestras aplicaciones. Los programadores realizan esta tarea para rastrear eventos, establecer plazos y generar cronogramas.

## Cómo:

Para calcular una fecha en el futuro o pasado en Gleam, podemos utilizar la función `DateTime.add` combinada con una cantidad de tiempo en el formato adecuado usando los operadores `seconds`, `minutes`, `hours`, `days`, `weeks` y `years`. Por ejemplo, si queremos obtener la fecha de 5 días después de hoy, podemos escribir:

```Gleam
DateTime.add(3 |> days)
```

El resultado sería la fecha en tres días a partir de hoy. También podemos restar tiempo usando el mismo método, solo utilizando números negativos. Por ejemplo, si queremos obtener la fecha de 10 días antes de hoy, podemos escribir:

```Gleam
DateTime.add(-10 |> days)
```

El resultado sería la fecha hace diez días a partir de hoy.

Otra forma de especificar una fecha es utilizando la función `DateTime.from_posix_utc`, que toma un timestamp POSIX (el número de segundos desde el 1 de enero de 1970 en UTC) y devuelve una representación de fecha y hora en formato `DateTime`.

## Profundizando:

Antes de la introducción de los timestamps POSIX, muchos sistemas utilizaban el formato de fecha y hora conocido como "Época", que establecía el comienzo de la hora como el 1 de enero de 1900. Sin embargo, esto presentaba algunos problemas, como la incapacidad de representar fechas antes de esa época. El uso de timestamps POSIX ha resuelto estas limitaciones y se ha convertido en el estándar para la gestión de tiempo en la mayoría de los sistemas.

Alternativamente, en lugar de utilizar la función `DateTime.add` en conjunto con la cantidad de tiempo deseada, también podemos especificar una fecha exacta utilizando la función `DateTime.from_components`, que toma los componentes de una fecha (año, mes, día) y devuelve una representación de fecha y hora en formato `DateTime`.

## Ver también:

Para obtener más información sobre el uso de fechas y horas en Gleam, puedes consultar la documentación oficial en https://gleam.run/documentation/#date-and-time. También puedes explorar la función `DateTime` y sus variantes en la biblioteca estándar de Gleam en https://github.com/gleam-lang/gleam_stdlib/blob/master/lib/DateTime.gleam.