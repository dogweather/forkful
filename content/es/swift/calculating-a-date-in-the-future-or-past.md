---
title:                "Swift: Calculando una fecha en el futuro o pasado"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por Qué

¿Alguna vez has necesitado saber en qué día caerá tu cumpleaños dentro de cinco años? ¿O cuántos días faltan para Navidad? Calcular fechas en el futuro o en el pasado es una tarea común en programación y en este artículo te enseñaremos cómo hacerlo en Swift.

## Cómo

Calculando una fecha en el futuro o en el pasado es una tarea relativamente sencilla en Swift gracias a las funciones que nos proporciona la clase `Date`.

Primero, necesitamos crear una instancia de la fecha actual utilizando la función `Date()`. A partir de esta instancia, podemos utilizar el método `addingTimeInterval()` para agregar un intervalo de tiempo a la fecha actual y así obtener una fecha en el futuro o en el pasado. Por ejemplo, si queremos calcular la fecha dentro de un año, podemos escribir:

```Swift
let unAnoEnSegundos: TimeInterval = 365 * 24 * 60 * 60 // segundos en un año
let fechaEnUnAno = Date().addingTimeInterval(unAnoEnSegundos)

print(fechaEnUnAno) // salida: 2021-07-04 19:57:34 +0000
```

Como puedes ver, la salida es una fecha con un año más que la fecha actual. Podemos utilizar esta misma lógica para calcular fechas en el pasado o para agregar diferentes intervalos de tiempo como días, horas, minutos, etc.

Otra forma de calcular fechas en el futuro o en el pasado es utilizando la clase `Calendar`. Esta clase nos permite manejar fechas y realizar operaciones con ellas. Por ejemplo, si queremos obtener la fecha dentro de un mes, podemos escribir lo siguiente:

```Swift
let fechaHoy = Date()
let fechaEnUnMes = Calendar.current.date(byAdding: .month, value: 1, to: fechaHoy)!

print(fechaEnUnMes) // salida: 2020-08-04 19:57:34 +0000
```

En este caso, estamos utilizando el método `date(byAdding:value:to:)` para agregar un mes a la fecha actual.

## Deep Dive

Si quieres profundizar más en el tema, puedes investigar sobre el manejo de fechas en Swift y cómo utilizar la clase `DateComponents` para realizar operaciones más avanzadas y precisas con fechas.

También es importante tener en cuenta la zona horaria y el formato de fecha al trabajar con fechas en Swift. Puedes investigar más sobre cómo manejar estos aspectos en tu código.

## Ver También

- [Documentación oficial de Swift sobre la clase Date](https://developer.apple.com/documentation/foundation/date)
- [Tutoriales de manejo de fechas en Swift](https://www.raywenderlich.com/6740393-swift-date-how-to-work-with-dates-in-swift)

¡Esperamos que este artículo te haya sido útil y te ayude a trabajar con fechas en Swift de manera más eficiente! Recuerda siempre utilizar las funciones y métodos apropiados para obtener resultados precisos y asegurarte de manejar las fechas correctamente en tu código. ¡Hasta la próxima!