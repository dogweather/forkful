---
title:                "Swift: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Por qué calcular una fecha en el futuro o en el pasado?

A veces, en la programación, necesitamos obtener una fecha en el futuro o en el pasado para realizar determinadas tareas. Por ejemplo, mostrar la fecha de un evento que sucederá en 3 días o calcular la edad de una persona basándonos en su fecha de nacimiento. En este artículo, te explicaremos cómo puedes calcular fácilmente una fecha en el futuro o en el pasado utilizando Swift.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado en Swift, podemos utilizar la clase `Date` y sus métodos `addingTimeInterval` y `addingComponents`. Veamos algunos ejemplos:

```Swift
// Crear una fecha actual
let fechaActual = Date()

// Mostrar la fecha de mañana
let fechaManana = fechaActual.addingTimeInterval(24 * 60 * 60)
print(fechaManana) // Output: 2020-04-01 18:00:00 +0000

// Mostrar la fecha de hace una semana
let fechaHaceUnaSemana = fechaActual.addingTimeInterval(-7 * 24 * 60 * 60)
print(fechaHaceUnaSemana) // Output: 2020-03-25 18:00:00 +0000

// Crear un dateComponent con 2 días y 3 horas
var dateComponent = DateComponents()
dateComponent.day = 2
dateComponent.hour = 3
// Mostrar la fecha en 2 días y 3 horas
let fechaEnDosDias = Calendar.current.date(byAdding: dateComponent, to: fechaActual)
print(fechaEnDosDias!) // Output: 2020-04-03 21:00:00 +0000
```

En los ejemplos anteriores, utilizamos `addingTimeInterval` para agregar o restar segundos a una fecha. También podemos utilizar `addingComponents` para agregar componentes específicos, como días, horas o meses. Ten en cuenta que, para obtener el resultado final, debemos usar el método `date(byAdding:to:)` de la clase `Calendar` para aplicar el componente agregado a la fecha actual.

## Profundizando

Ahora que sabemos cómo calcular una fecha en el futuro o en el pasado, es importante entender cómo funciona internamente la clase `Date` en Swift. Esta clase representa una fecha y hora específica en formato UTC (Coordinated Universal Time) y se basa en un contador de segundos desde la medianoche del 1 de enero de 1970. Al agregar o restar tiempo a una fecha, lo que estamos haciendo es sumar o restar segundos a ese contador. También podemos especificar componentes, como días, meses y años, para obtener una fecha más precisa.

Es importante tener en cuenta que cuando trabajamos con fechas y horas, siempre debemos tener en cuenta la zona horaria. En Swift, podemos especificar la zona horaria utilizando el objeto `TimeZone`. Si no especificamos la zona horaria, por defecto se utilizará la zona horaria del dispositivo en el que se está ejecutando la aplicación.

## Ver también

- Documentación oficial de Apple sobre la clase `Date` en Swift: https://developer.apple.com/documentation/foundation/date
- Artículo sobre cómo trabajar con fechas y horas en Swift: https://www.dummies.com/programming/swift/how-to-work-with-dates-and-times-in-swift/
- Ejemplo de aplicación utilizando la clase `Date` en Swift: https://www.hackingwithswift.com/example-code/system/how-to-create-a-date-the-swift-way