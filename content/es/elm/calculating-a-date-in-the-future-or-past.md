---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Elm: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Calcular una fecha futura o pasada se trata de agregar o eliminar días, meses o años a la fecha actual. Los programadores hacen esto para programar eventos, calcular fechas de caducidad, entre otros usos.

## Cómo hacerlo:
Ahora podemos aprender cómo hacer esto con Elm en ejemplos rápidos y prácticos.

```Elm
import Time

-- Definir fecha actual
currDate : Time.Posix
currDate =
    Time.millisToPosix 1580515200000

-- Calcular una fecha futura (suma 10 días)
futureDate : Time.Posix
futureDate =
    Time.plus currDate (Time.days 10)

-- Calcular una fecha pasada (resta 10 días)
pastDate : Time.Posix
pastDate =
    Time.minus currDate (Time.days 10)
```
Salida:

```
Fecha actual: 2020-02-01
Fecha Futura: 2020-02-11
Fecha Pasada: 2020-01-22
```

## Inmersión profunda:
La manipulación de fechas ha sido una herramienta crítica en la programación desde sus primeros días. En Elm, usamos el módulo Time para trabajar con fechas y horas.

Alternativamente, algunos podrían optar por bibliotecas externas como 'elm-date-extra', pero esto agrega una dependencia adicional a su proyecto.

Al calcular fechas futuras o pasadas, Elm convierte la fecha actual en milisegundos, luego agrega o resta la cantidad especificada de milisegundos (convertido desde días, meses, etc) y luego vuelve a convertir el resultado en una fecha.

## Ver también:
1. Documentación de Elm Time: `https://package.elm-lang.org/packages/elm/time/latest/`
2. Guía de Elm para manipulación de fechas: `https://korban.net/posts/elm/2018-02-24-elm-date-and-time-basics/`
3. Paquete elm-date-extra: `https://package.elm-lang.org/packages/rluiten/elm-date-extra/latest/`