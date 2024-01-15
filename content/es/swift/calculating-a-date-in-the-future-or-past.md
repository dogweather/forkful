---
title:                "Calculando una fecha en el futuro o en el pasado."
html_title:           "Swift: Calculando una fecha en el futuro o en el pasado."
simple_title:         "Calculando una fecha en el futuro o en el pasado."
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

Calcular la fecha en el futuro o pasado puede ser útil en varias situaciones, como por ejemplo, planificar eventos o programar tareas. También puede ser útil en aplicaciones de calendario o de seguimiento de fechas importantes.

## Cómo

Para calcular una fecha en el futuro o pasado en Swift, se puede utilizar la clase `Date` y el método `addingTimeInterval`. Por ejemplo, si queremos calcular la fecha de mañana, podemos hacer lo siguiente:

```Swift
let currentDate = Date()
let tomorrow = currentDate.addingTimeInterval(86400) // 86400 segundos en un día
print(tomorrow) // 2021-09-02 03:00:00 +0000
```

También podemos utilizar valores negativos para calcular una fecha en el pasado. Por ejemplo, si queremos calcular la fecha de hace una semana, podemos hacer lo siguiente:

```Swift
let currentDate = Date()
let lastWeek = currentDate.addingTimeInterval(-604800) // 604800 segundos en una semana
print(lastWeek) // 2021-08-26 03:00:00 +0000
```

Es importante tener en cuenta que estos cálculos se basan en la fecha y hora actual del dispositivo en el que se está ejecutando la aplicación. Si se desea obtener una fecha en una zona horaria específica, se pueden utilizar las clases `DateFormatter` y `TimeZone`.

## Profundizando

La clase `Date` en Swift se basa en el sistema de tiempo Unix, que cuenta los segundos desde el 1 de enero de 1970. Por lo tanto, al agregar un intervalo de tiempo a una fecha, se está agregando esa cantidad de segundos al valor de tiempo de la fecha actual.

Para realizar cálculos más precisos, se pueden utilizar las clases `Calendar` y `DateComponents`. Estas permiten especificar intervalos de tiempo en unidades más grandes, como días, meses o años. Por ejemplo, si queremos calcular la fecha de dentro de 6 meses, podemos hacer lo siguiente:

```Swift
let calendar = Calendar.current
let currentDate = Date()
let sixMonthsFromNow = calendar.date(byAdding: .month, value: 6, to: currentDate)
print(sixMonthsFromNow) // 2022-02-28 03:00:00 +0000
```

También se pueden utilizar estas clases para realizar cálculos más complejos, como tener en cuenta los días hábiles o los días festivos.

## Ver también

- Documentación oficial de Swift sobre la clase `Date`: https://developer.apple.com/documentation/foundation/date
- Guía básica de calendarios y fechas en Swift: https://www.raywenderlich.com/731-minecraft-calendar-swift-tutorial-how-to-get-started-with-calendars-and-dates
- Ejemplo de uso de `DateComponents` para calcular la diferencia entre dos fechas: https://www.appcoda.com/swift-date-components/