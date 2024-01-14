---
title:    "Swift: Calculando una fecha en el futuro o pasado"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por qué

Calcular una fecha en el futuro o en el pasado es una tarea común en la programación. Ya sea para planificar citas o eventos, o simplemente para realizar cálculos basados en fechas, saber cómo calcular una fecha es una habilidad útil que todo programador debe tener.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado en Swift, utilizaremos la estructura `Date` y el objeto `DateComponents`. Primero, debemos especificar los componentes de la fecha que queremos calcular, como el año, el mes, el día y la hora. Luego, podemos utilizar el método `Calendar.date(byAdding:to:)` para sumar o restar estos componentes de una fecha existente.

Por ejemplo, si queremos calcular la fecha 2 de abril de 2022, podemos hacerlo de la siguiente manera:

```Swift
let date = Date() // fecha actual
var components = DateComponents()
components.day = 2
components.month = 4
components.year = 2022

let futureDate = Calendar.current.date(byAdding: components, to: date) // 02/04/2022
```
Además de poder sumar o restar componentes, también podemos especificar qué tipo de componente queremos calcular. Por ejemplo, si sólo queremos calcular la fecha un mes en el futuro, podemos hacerlo de la siguiente manera:

```Swift
let date = Date() // fecha actual
var components = DateComponents()
components.month = 1 // añadimos 1 mes a la fecha actual

let futureDate = Calendar.current.date(byAdding: components, to: date) // fecha actual + 1 mes
```

## Inmersión profunda

Una cosa a tener en cuenta al calcular fechas en el futuro o en el pasado es que tenemos que tomar en cuenta los diferentes calendarios y zonas horarias. Por defecto, Swift utiliza el calendario Gregoriano y la zona horaria del sistema. Sin embargo, podemos especificar otros calendarios y zonas horarias según sea necesario utilizando el objeto `Calendar`.

Además, podemos realizar cálculos más complejos utilizando el objeto `DateComponents`, como la diferencia entre dos fechas, obtener el día de la semana, o incluso calcular el tiempo transcurrido entre dos fechas.

## Ver también

- [Documentación oficial de Apple sobre el objeto DateComponents](https://developer.apple.com/documentation/foundation/datecomponents)
- [Guía del calendario en Swift](https://www.hackingwithswift.com/articles/118/how-to-use-dates-and-dateformatter-in-swift)
- [Cómo trabajar con zonas horarias en Swift](https://www.raywenderlich.com/5154-programming-for-i18n-and-l10n-in-swift)