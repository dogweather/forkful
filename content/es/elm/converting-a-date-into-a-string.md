---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "Elm: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una fecha en una cadena de texto?

Si estás trabajando en un proyecto en Elm que involucra fechas, es probable que en algún momento necesites convertir una fecha en una cadena de texto. Por ejemplo, al mostrar una fecha en un formato específico o al guardar una fecha en una base de datos. En este artículo, te enseñaré cómo hacerlo de forma sencilla y eficiente en Elm.

## Cómo hacerlo

Para convertir una fecha en una cadena de texto en Elm, necesitamos usar la función `Date.toString` del paquete `elm/time`. Esta función acepta una cadena de formato y una fecha y devuelve la fecha en el formato especificado.

Veamos un ejemplo de cómo podemos usar esta función. Supongamos que tenemos una fecha en formato Unix en una variable llamada `unixDate` y queremos mostrarla en formato DD/MM/AAAA. Podríamos hacerlo de la siguiente manera:

```elm
import Time exposing (Date)

unixDate = 1618920000

dateToString : String
dateToString =
    unixDate
        |> Date.fromTime
        |> Result.map (\date -> Date.toString "dd/MM/yyyy" date)
        |> Result.withDefault ""
```

En este ejemplo, usamos la función `Date.fromTime` para convertir nuestro valor de Unix en un tipo de dato `Date` y luego pasamos ese valor a la función `Date.toString` junto con nuestro formato deseado. Por último, utilizamos la función `Result.withDefault` para manejar el caso en el que la conversión falla y asignamos una cadena vacía como valor por defecto.

Al imprimir el valor de `dateToString`, obtendremos la fecha en formato DD/MM/AAAA, en este caso: 19/04/2021.

## Deep Dive

La función `Date.toString` en realidad acepta una variedad de formatos diferentes. Puedes encontrar una lista completa de los formatos disponibles en la documentación del paquete `elm/time`. Aquí te dejo algunos ejemplos para que puedas probar:

- `"yyyy-MM-dd"`: Muestra la fecha en formato AAAA-MM-DD, por ejemplo: 2021-04-19.
- `"MMMM dd, yyyy"`: Muestra la fecha en formato mes dia, año, por ejemplo: April 19, 2021.
- `"ddd, d MMM yyyy HH:mm:ss z"`: Muestra la fecha en formato día de la semana, día mes año hora:minuto:segundo zona horaria, por ejemplo: Mon, 19 Apr 2021 00:00:00 UTC.

Además de la función `Date.toString`, también puedes utilizar la función `Date.toIsoString` para obtener la fecha en un formato ISO 8601, que es ampliamente utilizado para representar fechas en intercambios de datos.

## Ver también

- [Documentación oficial de Elm para el módulo Time](https://package.elm-lang.org/packages/elm/time/latest/)
- [Formato ISO 8601 en Wikipedia](https://es.wikipedia.org/wiki/ISO_8601)