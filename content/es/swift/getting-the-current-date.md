---
title:                "Obteniendo la fecha actual"
html_title:           "Swift: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

Si estás creando una aplicación o un proyecto que necesita mostrar la fecha actual, es importante saber cómo obtenerla en tu código. Esto te permite mantener la información actualizada y brindar una experiencia de usuario más precisa y personalizada.

## Cómo hacerlo

Para obtener la fecha actual en Swift, puedes utilizar la clase `Date`, que representa una fecha y hora específicas. A continuación, se muestra un ejemplo de cómo obtener la fecha actual en formato de cadena:

```Swift 
let fechaActual = Date()
let formatter = DateFormatter()
formatter.dateFormat = "dd/MM/yyyy"
let fechaActualString = formatter.string(from: fechaActual)
print(fechaActualString)
```
**Salida:**
31/10/2021

En este ejemplo, primero creamos una instancia de la clase `Date()` que representa la fecha y hora actuales. Luego, utilizamos un objeto de la clase `DateFormatter()` para especificar el formato en el que queremos mostrar la fecha. En este caso, establecemos el formato en "dd/MM/yyyy" para obtener la fecha en formato día/mes/año. Finalmente, utilizamos el método `string(from:)` para convertir la fecha en formato de cadena y mostrarla en la consola.

## Profundizando

Existen diferentes propiedades y métodos que puedes utilizar con la clase `Date()`, dependiendo de lo que necesites en tu proyecto. Por ejemplo, puedes obtener la hora actual utilizando `let horaActual = Date()`, o incluso agregar o restar días, semanas, meses o años a una fecha específica utilizando métodos como `addingTimeInterval(_: TimeInterval)`. También puedes comparar fechas y obtener la diferencia entre ellas utilizando métodos como `compare(_: Date)` y `timeIntervalSince(_: Date)`.

Recuerda que la fecha y hora actual pueden variar según la ubicación de tu dispositivo, por lo que es importante tener en cuenta la zona horaria en la que se encuentra el usuario y ajustarla si es necesario.

## Ver también

- [Documentación de Swift sobre la clase Date](https://developer.apple.com/documentation/foundation/date)
- [Cómo trabajar con fechas y horas en Swift](https://www.hackingwithswift.com/example-code/language/how-to-work-with-dates-and-times-in-swift)