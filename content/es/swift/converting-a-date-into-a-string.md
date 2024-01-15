---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "Swift: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué 

La conversión de una fecha en una cadena de texto es una habilidad esencial en el desarrollo de aplicaciones Swift. Al convertir una fecha, podemos mostrarla en un formato legible para el usuario o almacenarla en una base de datos en formato de texto.

## Cómo hacerlo

Para convertir una fecha en una cadena de texto en Swift, utilizaremos el tipo de dato `Date` y la clase `DateFormatter`. Primero, crearemos una instancia de `DateFormatter` y especificaremos el estilo de formato que queremos para nuestra fecha. Luego, usaremos el método `string(from: Date)` de `DateFormatter` para convertir nuestra fecha en una cadena de texto.

```Swift
// Crear una fecha
let fecha = Date()

// Crear una instancia de DateFormatter
let dateFormatter = DateFormatter()

// Especificar el estilo de formato deseado
dateFormatter.dateStyle = .long

// Convertir la fecha en una cadena de texto
let fechaCadena = dateFormatter.string(from: fecha)

print(fechaCadena) // "20 de abril de 2021"
```

Podemos personalizar aún más el formato de nuestra fecha cambiando el estilo de formato o especificando una plantilla de formato personalizada. Por ejemplo, podemos mostrar la fecha en un formato de 24 horas utilizando la plantilla "HH:mm" en lugar del estilo de formato "long".

```Swift
// Especificar una plantilla de formato personalizada
dateFormatter.dateFormat = "HH:mm"

let horaCadena = dateFormatter.string(from: fecha)

print(horaCadena) // "16:30"
```

## Profundizando

Además de mostrar fechas en diferentes formatos, también podemos convertir fechas a cadenas de texto en diferentes idiomas. Para ello, utilizaremos el método `setLocalizedDateFormatFromTemplate(_: String)` de `DateFormatter` y especificaremos el idioma deseado.

```Swift
// Especificar idioma deseado
dateFormatter.setLocalizedDateFormatFromTemplate("MMMddYYYY")
dateFormatter.locale = Locale(identifier: "es")

let fechaCadena = dateFormatter.string(from: fecha)

print(fechaCadena) // "may. 10, 2021"
```

También podemos agregar símbolos especiales a nuestras cadenas de formato para mostrar información como la hora, los minutos, etc. Mira la documentación de Apple para obtener una lista completa de estos símbolos y cómo utilizarlos en una plantilla de formato personalizada.

## Ver también 

- [La clase DateFormatter en la documentación de Apple](https://developer.apple.com/documentation/foundation/dateformatter)
- [Cómo trabajar con fechas en Swift en Medium](https://medium.com/swlh/working-with-dates-in-swift-53fa7307a067)
- [Manejo de fechas en Swift: locales y formatos de fecha en Ray Wenderlich](https://www.raywenderlich.com/8176546-dates-and-times-in-swift-getting-started)