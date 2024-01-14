---
title:                "Swift: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por Qué

 ¿Alguna vez has necesitado mostrar la fecha en tu aplicación de Swift de una manera más legible y fácil de entender? Convertir una fecha en una cadena es una habilidad importante en la programación y te permite personalizar cómo la fecha es presentada al usuario. ¡Sigue leyendo para aprender cómo hacerlo!

## Cómo Hacerlo

Para convertir una fecha en una cadena en Swift, necesitarás utilizar un objeto DateFormatter. Esto permite definir el formato en el cual quieres mostrar la fecha. A continuación, un ejemplo de cómo convertir una fecha en una cadena con el formato "Mes Día, Año":

```
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "MMMM d, yyyy"

let date = Date()
let dateString = dateFormatter.string(from: date)
print(dateString) // Output: August 13, 2020
```

En este ejemplo primero creamos un objeto DateFormatter y luego definimos el formato deseado en la propiedad `dateFormat`. Luego, utilizamos el método `string(from:)` para convertir la fecha actual en una cadena utilizando el formato que especificamos. Finalmente, imprimimos la cadena en consola para verificar el resultado.

Pero los formatos no se limitan a sólo "Mes Día, Año", puedes personalizarlo según tus necesidades utilizando una combinación de letras y símbolos que representan diferentes partes de la fecha como el día, mes, año, etc. Puedes encontrar una lista completa de estos símbolos en la documentación de DateFormatter.

## Deep Dive

Además de definir un formato personalizado para la fecha, también puedes cambiar el idioma y la región en la que se presenta. Esto es muy útil si tienes una aplicación que se utiliza en diferentes países con diferentes formatos de fecha. Para hacer esto, puedes utilizar la propiedad `locale` del objeto DateFormatter. Por ejemplo, para mostrar la fecha en español con el formato "Día Mes Año":

```
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd MMMM yyyy"

let date = Date()
dateFormatter.locale = Locale(identifier: "es_ES")

let dateString = dateFormatter.string(from: date)
print(dateString) // Output: 13 agosto 2020
```

## Ver también

- Documentación de DateFormatter: https://developer.apple.com/documentation/foundation/dateformatter
- Tutorial sobre cómo trabajar con fechas en Swift: https://www.raywenderlich.com/6403-dates-and-times-in-swift-getting-started
- Video tutorial sobre cómo convertir una fecha en una cadena en Swift: https://www.youtube.com/watch?v=-juxCxnf-kA