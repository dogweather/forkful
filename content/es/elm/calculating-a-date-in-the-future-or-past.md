---
title:                "Elm: Calculando una fecha en el futuro o en el pasado"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué
¿Alguna vez has necesitado calcular una fecha en el futuro o en el pasado? Puede ser para programar una tarea en tu calendario o para mostrar una fecha específica en una aplicación. En cualquier caso, saber cómo calcular fechas en Elm te será de gran ayuda.

## Cómo hacerlo
Lo primero que debes tener en cuenta es que en Elm, las fechas se representan como `Date` en lugar de `DateTime` como en otros lenguajes. Para calcular una fecha en el futuro o en el pasado, podemos utilizar la función `add` que toma dos argumentos: la unidad de tiempo que queremos agregar y la cantidad de esa unidad de tiempo. Veamos un ejemplo:

```elm
import Date exposing (..)

-- Vamos a calcular la fecha de hoy en dos días
hoy = Date.fromText "2021-10-14"
fechaFutura = add Days 2 hoy 
-- salida: 2021-10-16
```

En este ejemplo, estamos importando el módulo `Date` y utilizando la función `fromText` para crear una fecha a partir de una cadena de texto específica en formato ISO, en este caso, "2021-10-14". Luego, utilizando la función `add` agregamos `2` días a esa fecha, lo que nos da una nueva fecha en el futuro.

También podemos calcular fechas en el pasado utilizando números negativos como `add Days -2 hoy` que nos devolvería "2021-10-12".

Además de `Days`, también podemos utilizar otras unidades de tiempo como `Minutes`, `Hours`, `Months`, `Years`, entre otras.

## Profundizando
En Elm, cada vez que se realiza una operación de fecha, se crea una nueva instancia de `Date` en lugar de modificar la existente. Esto permite tener un código más seguro ya que evita mutaciones inesperadas. Además, Elm cuenta con un módulo `Time` que permite trabajar con intervalos de tiempo en lugar de fechas específicas.

Otra función útil para calcular fechas es `diff` que nos permite calcular la diferencia en días entre dos fechas. Por ejemplo:

```elm
import Date exposing (..)

hoy = Date.fromText "2021-10-14"
ayer = Date.fromText "2021-10-13"

diferencia = diff hoy ayer
-- salida: 1
```

`diff` también tiene en cuenta el cambio de hora y funciona correctamente en diferentes zonas horarias.

## Ver también
- [Documentación oficial de Date en Elm](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Tutorial de Elm en español](https://elm-tutorial.org/es/)

Ahora que sabes cómo calcular fechas en Elm, ¡puedes utilizar esta habilidad en tus proyectos o resolver problemas en tus aplicaciones! Recuerda siempre optar por una programación funcional y segura con Elm. ¡Hasta la próxima!