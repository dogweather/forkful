---
title:                "Swift: Obteniendo la fecha actual."
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

##¿Por qué deberías obtener la fecha actual?

Obtener la fecha actual es una tarea común en la programación, ya que puede ser útil para rastrear la hora y fecha en que ocurrió un evento o para mostrar información actualizada en una aplicación. En Swift, hay varias formas de obtener la fecha actual, lo que nos brinda flexibilidad en cómo la podemos usar en nuestras aplicaciones.

##Cómo hacerlo:

Para obtener la fecha actual en Swift, podemos utilizar la estructura `Date` y su método `init()`. Este método devuelve un objeto `Date` con la fecha y hora actuales. Veamos un ejemplo de cómo podemos obtener la fecha y hora actual y mostrarla en la consola:

```Swift
let currentDate = Date()

print(currentDate)
```

Salida:
>2021-08-10 15:27:59 +0000

También podemos personalizar el formato en el que se muestra la fecha y hora utilizando la clase `DateFormatter`. Este permite especificar un formato específico para mostrar la fecha y hora actual. Por ejemplo, si queremos mostrar solo la hora y los minutos, podemos usar el siguiente código:

```Swift
let formatter = DateFormatter()
formatter.dateFormat = "HH:mm"

let currentTime = formatter.string(from: Date())

print(currentTime)
```

Salida:
>15:30

##Profundizando:

La estructura `Date` en Swift es en realidad un alias para `DateInterval`, que representa un intervalo de tiempo entre dos fechas. También podemos usar la clase `Calendar` para realizar operaciones con fechas, como calcular la diferencia de tiempo entre dos fechas o agregar o restar un cierto número de días a una fecha.

Además, si trabajamos con diferentes zonas horarias, podemos utilizar la clase `TimeZone` para ajustar la fecha y hora según la zona horaria especificada.

Es importante tener en cuenta que la fecha y hora actual se basa en el dispositivo en el que se está ejecutando la aplicación, por lo que puede variar dependiendo de la ubicación del dispositivo y la hora establecida en el sistema.

##Ver también:

- La documentación oficial de Apple sobre la estructura `Date` en Swift: https://developer.apple.com/documentation/foundation/date
- Un tutorial sobre cómo trabajar con fechas y horas en Swift: https://www.hackingwithswift.com/articles/117/the-ultimate-guide-to-date-and-time-programming-with-swift
- Un video tutorial en español sobre cómo obtener la fecha actual en Swift: https://www.youtube.com/watch?v=zQwHE5y5vfg