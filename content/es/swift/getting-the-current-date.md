---
title:                "Swift: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué obtener la fecha actual es importante en Swift

Obtener la fecha actual puede ser muy útil en el desarrollo de aplicaciones en Swift. Puede ser utilizado para saber cuándo se creó o modificó un archivo, para mostrar la fecha en una interfaz de usuario, o para realizar cálculos basados en la fecha. En este artículo, exploraremos cómo obtener la fecha actual en Swift y cómo puedes aprovechar esta funcionalidad en tus propias aplicaciones.

## Cómo hacerlo en Swift

Para obtener la fecha actual en Swift, puedes utilizar la clase `Date` y el formato ISO 8601. El código se verá así:

```Swift
let date = Date()
let formatter = ISO8601DateFormatter()
let currentDate = formatter.string(from: date)
print(currentDate)
```

El resultado de este código será la fecha y hora actual en formato ISO 8601, que se verá así: `2020-08-20T10:30:05-05:00`. Este formato es útil ya que es ampliamente utilizado y fácil de convertir a otros formatos si es necesario.

Una vez que tengas la fecha actual en una variable, puedes realizar diferentes operaciones como compararla con otra fecha, sumar o restar días, o mostrar solo la fecha o la hora. A continuación, se muestra un ejemplo de cómo obtener solo la fecha actual en formato `dd/MM/yyyy`:

```Swift
let date = Date()
let formatter = DateFormatter()
formatter.dateFormat = "dd/MM/yyyy"
let currentDate = formatter.string(from: date)
print(currentDate)
```

El resultado de este código será la fecha actual en formato `dd/MM/yyyy`, que se verá así: `20/08/2020`.

## Profundizando en obtener la fecha actual

En Swift, la clase `Date` es en realidad un alias para la estructura de `Foundation.Date`. Esta estructura representa un solo punto en el tiempo y se basa en una marca de tiempo en segundos. Esto significa que cuando obtienes la fecha actual, obtienes un objeto que contiene información sobre la fecha y hora en ese momento exacto.

Si quieres obtener la fecha actual en una zona horaria específica, puedes utilizar la propiedad `timeZone` de `DateFormatter` y proporcionar la zona horaria como un parámetro. También puedes utilizar el método `setTimeZone` de `DateFormatter` para cambiar la zona horaria después de haber definido el formato de fecha.

## Ver también

- [Documentación de Apple sobre la clase `Date`](https://developer.apple.com/documentation/foundation/date)
- [Documentación de Apple sobre el formato ISO 8601](https://developer.apple.com/documentation/foundation/iso8601dateformatter)
- [Guía de fecha y hora en Swift](https://www.hackingwithswift.com/articles/117/the-ultimate-guide-to-date-and-time-programming-with-swift)