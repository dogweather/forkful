---
title:                "Comparando dos fechas"
html_title:           "Elm: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Comparar dos fechas es un proceso común en la programación. Consiste en evaluar si una fecha es igual, anterior o posterior a otra fecha. Los programadores hacen esto para ordenar eventos, filtrar datos o verificar si una tarea está atrasada o no.

## ¡Cómo hacerlo!
Usando la función `compare` de Elm, podemos comparar dos fechas y obtener un resultado de `LT` (menor),`GT` (mayor) o `EQ` (igual). Por ejemplo:

```Elm
compare May 15 2021 May 20 2021
```

El resultado sería `LT`, ya que la fecha del primero de Mayo es anterior a la fecha del 20 de Mayo.

Para comparar fechas con mayor precisión, podemos utilizar funciones como `toTime` para convertir una fecha a un valor numérico y luego compararlos. Por ejemplo:

```Elm
compare (toTime May 15 2021) (toTime May 20 2021)
```

El resultado seguirá siendo `LT`, pero ahora se consideran las horas, minutos y segundos de las fechas.

## Profundizando más
La necesidad de comparar fechas surge de la importancia de tener un orden en las tareas y eventos. La función `compare` se basa en el estándar ISO 8601, que define el formato de fecha y hora internacionalmente aceptado.

Otras alternativas para comparar fechas incluyen el uso de bibliotecas externas como `date-extra` o funciones personalizadas creadas por los propios programadores.

La implementación de la función `compare` en Elm se basa en una comparación iterativa de los componentes de la fecha, comenzando por el año y continuando con el mes, día, hora, minutos y segundos.

## Véase también
- Documentación oficial de Elm: [Comparing two values](https://elm-lang.org/docs/syntax#comparing-values)
- Especificación ISO 8601: [Data elements and interchange formats](https://www.iso.org/standard/70907.html)