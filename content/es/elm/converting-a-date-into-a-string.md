---
title:    "Elm: Convirtiendo una fecha en una cadena"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

La conversión de una fecha en una cadena de texto es una habilidad útil en la programación, especialmente al trabajar con datos de tiempo y fechas. En este artículo, exploraremos cómo realizar esta conversión en Elm.

## Cómo hacerlo

Primero, debemos importar el paquete `Time` en nuestro proyecto Elm. Luego, utilizando la función `toIsoString`, podemos convertir una fecha en una cadena de texto con el formato ISO 8601.

```Elm
import Time exposing (toIsoString)

date = Date.fromIsoString "2021-05-11"

toIsoString date -- "2021-05-11T00:00:00.000"
```

También podemos utilizar la función `format` para personalizar el formato de la cadena de texto según nuestras necesidades. Esta función utiliza la sintaxis de formato de fecha `yy-MM-dd` para especificar cómo queremos que se vea nuestra cadena de texto.

```Elm
import Time exposing (format)

date = Date.fromIsoString "2021-05-11"

format "yy-MM-dd" date -- "21-05-11"
```

Además, podemos utilizar la función `inLocalZone` para convertir la fecha en la zona horaria local del usuario.

```Elm
import Time exposing (inLocalZone)

date = Date.fromIsoString "2021-05-11"

inLocalZone date -- "Tue, 11 May 2021 00:00:00 GMT-0400 (Eastern Daylight Time)"
```

## Sumergirse más profundo

Hay muchas más funciones y opciones disponibles en el paquete `Time` para manipular y convertir fechas. Podemos utilizar la función `fromCalendarDate` para crear una fecha a partir de un año, mes y día específicos, o la función `toTime` para convertir una fecha en milisegundos desde la época de Unix. Es importante revisar la documentación del paquete para conocer todas las posibilidades.

## Ver también

- Documentación del paquete Time: https://package.elm-lang.org/packages/elm/time/latest/
- Tutorial de Elm en español: https://elmprogramming.com/es/