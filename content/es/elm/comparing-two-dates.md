---
title:                "Comparando dos fechas"
html_title:           "C#: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Comparando fechas en Elm: Un vistazo

## ¿Qué y por qué?

Comparar dos fechas significa determinar cuál es anterior o posterior. Los programadores tienden a hacer esto al trabajar con eventos registrados, procesos de programación y funcionalidades basadas en tiempo.

## Cómo se hace:

Aquí tienes un ejemplo de cómo puedes comparar dos fechas en Elm:

```Elm
import Time exposing (Date, Day)

fecha1 : Date
fecha1 = 
    Time.fromCalendarDate 2021 Day.October 1

fecha2 : Date
fecha2 = 
    Time.fromCalendarDate 2022 Day.February 1

compararResultado : Order
compararResultado = 
    compare fecha1 fecha2
```

Cuando se ejecuta este código, `compararResultado` será `LT`, lo que significa que `fecha1` es anterior a `fecha2`.

## Un vistazo más profundo

Desde un punto de vista histórico, Elm convierte las fechas en milisegundos desde la época UNIX para poder compararlas. Esto sirve para lidiar con la zona horaria del sistema operativo nativo.

Por otro lado, si estás buscando alternativas, también puedes usar funciones de biblioteca como `isBefore` y `isAfter`. Estas funciones encapsulan la lógica de la comparación y pueden hacer que tu código sea más legible.

Sobre detalles de implementación, es importante mencionar que Elm usa la Semántica de Orden Total: dos fechas diferentes siempre serán consideradas como una "antes" y la otra "después", incluso si están en la misma milisegunda.

## Ver También

Te invito a revisar documentación relacionada para profundizar en el tema:

- La [documentación oficial de Elm sobre Time](https://package.elm-lang.org/packages/elm/time/latest/)