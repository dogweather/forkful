---
title:                "Elm: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

Alguna vez te has preguntado cómo saber la fecha en el futuro o en el pasado? Tal vez estés planeando una reunión importante o simplemente quieras saber cuántos días han pasado desde un evento especial. ¡En Elm, calcular fechas es muy sencillo y en este artículo te lo mostraré!

## Cómo

Para calcular una fecha en el futuro o en el pasado en Elm, utilizamos la función `Date.fromIsoString` y le pasamos una cadena de texto en formato ISO que contiene la fecha que queremos calcular. Por ejemplo, si queremos saber la fecha dentro de 10 días, podríamos hacer lo siguiente:

```elm
Date.fromIsoString "2022-01-01"
```

Esto nos devolvería un objeto `Result` con la fecha calculada, que luego podemos manipular según nuestras necesidades.

Si queremos obtener la fecha en el pasado, simplemente le restamos días a la fecha que pasamos como parámetro. Por ejemplo, si queremos la fecha 3 días atrás, nuestro código sería así:

```elm
Date.fromIsoString "2022-01-01" - 3
```

Y de nuevo, obtendríamos un objeto `Result` con la fecha calculada.

## Profundizando

¿Pero cómo funciona exactamente esta función? En realidad, Elm utiliza un sistema de unidades de tiempo en milisegundos desde el 1 de enero de 1970 para calcular fechas. Esto significa que si sumamos o restamos días a una fecha, lo que en realidad estamos haciendo es sumar o restar milisegundos.

Además, es importante tener en cuenta que Elm maneja fechas y horas en UTC (Tiempo Universal Coordinado), por lo que puede ser necesario convertir las fechas a la zona horaria local antes de mostrarlas al usuario.

## Ver también

Si quieres saber más sobre cómo trabajar con fechas en Elm, te recomiendo revisar la documentación oficial y estos otros recursos:

- [Documentación oficial de Elm sobre fechas y horas](https://package.elm-lang.org/packages/elm/time/latest/)
- [Ejemplos de código en línea para jugar con fechas en Elm](https://ellie-app.com/56bSWdhBMd5a1)
- [Tutorial en español sobre cómo trabajar con fechas en Elm](https://www.caraveo.com/posts/2020-06-01-working-with-dates-in-elm)