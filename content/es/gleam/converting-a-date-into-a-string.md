---
title:                "Gleam: Convirtiendo una fecha en una cadena"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una fecha en una cadena de texto es una tarea común en la programación. Puede ser útil para mostrar fechas en un formato específico en una aplicación o para trabajar con fechas en APIs. En Gleam, esto se puede lograr fácilmente utilizando la función `Date.to_string/1`.

## Cómo hacerlo

El primer paso para convertir una fecha en una cadena de texto en Gleam es crear una variable que contenga la fecha que deseamos convertir, utilizando la función `Date.new/4`.

```
Gleam
let fecha = Date.new(2021, 10, 15)
```

Luego, podemos llamar a la función `Date.to_string/1` y pasarle la variable de fecha como argumento. Esto devolverá una cadena de texto en formato ISO 8601.

```
Gleam
let fecha = Date.new(2021, 10, 15)
let fecha_string = Date.to_string(fecha)

```

El resultado de `fecha_string` sería `"2021-10-15"`. Sin embargo, si queremos un formato específico, podemos utilizar la función `Date.format/2` y especificar el formato deseado.

```
Gleam
let fecha = Date.new(2021, 10, 15)
let fecha_string = Date.format(fecha, "D, d MMM YYYY")

```

El resultado de `fecha_string` sería `"Fri, 15 Oct 2021"`, ya que hemos especificado el formato para mostrar el día de la semana, el día del mes, el mes y el año.

## Profundizando

La función `Date.to_string/1` utiliza el formato ISO 8601 por defecto, pero también se puede especificar otro formato utilizando la función `Date.format/2`. La documentación de Gleam proporciona una lista de los formatos de fecha y hora disponibles para utilizar.

También es importante tener en cuenta que, al igual que en otros lenguajes de programación, es necesario tener en cuenta las diferentes zonas horarias al trabajar con fechas y realizar conversiones a cadena de texto.

## Ver también

- [Documentación de Gleam sobre fechas](https://gleam.run/documentation/built-in-modules/date)
- [Ejemplos de formato de fecha y hora en Gleam](https://blog2021.gleam.run/?p=174)