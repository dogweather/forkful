---
date: 2024-01-20 17:30:42.251610-07:00
description: "C\xF3mo Hacerlo: Elm actualmente no incluye una biblioteca de manejo\
  \ de tiempo en su n\xFAcleo, por lo que necesitar\xE1s una librer\xEDa externa como\u2026"
lastmod: '2024-03-13T22:44:59.006872-06:00'
model: gpt-4-1106-preview
summary: "Elm actualmente no incluye una biblioteca de manejo de tiempo en su n\xFA\
  cleo, por lo que necesitar\xE1s una librer\xEDa externa como `justinmimbs/date`\
  \ para manejar fechas de manera efectiva."
title: Calcular una fecha en el futuro o pasado
weight: 26
---

## Cómo Hacerlo:
Elm actualmente no incluye una biblioteca de manejo de tiempo en su núcleo, por lo que necesitarás una librería externa como `justinmimbs/date` para manejar fechas de manera efectiva. Aquí te muestro cómo:

```Elm
import Date
import Date.Extra as Date

sumarDias : Date.Posix -> Int -> Date.Posix
sumarDias fecha dias = 
    fecha |> Date.add Days dias

-- Ejemplo de uso:
main =
    -- Supón que hoy es 1 de Enero 2023
    let
        hoy = Date.fromCalendarDate 2023 1 1 |> Maybe.withDefault Date.posixEpoch
        enDiezDias = sumarDias hoy 10
    in
    Date.toIsoString enDiezDias
    -- Salida: "2023-01-11"
```

## Inmersión Profunda:
Históricamente, el manejo de fechas ha sido complejo debido a distintos calendarios y cambios de hora. El paquete `justinmimbs/date` que usamos se basa en el estándar ISO 8601 y el tipo `Date.Posix` representa un momento en tiempo UTC. 

Las alternativas incluyen escribir tu propio manejador de tiempo o usar otra librería como `elm-time`. Implementar tu propia solución puede ser engorroso por las peculiaridades de los calendarios y las zonas horarias. Por ello, las bibliotecas de terceros son preferidas ya que manejan estos problemas minuciosos por ti.

## Ver También:
- Documentación del paquete `justinmimbs/date` en Elm: https://package.elm-lang.org/packages/justinmimbs/date/latest/
- Tutorial sobre manejo de tiempo en Elm: https://elmprogramming.com/date.html
- Página de `elm-time`, una alternativa a considerar: https://package.elm-lang.org/packages/elm/time/latest/
