---
title:                "Swift: Convirtiendo una fecha en una cadena"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una fecha en una cadena?

Con el creciente uso de aplicaciones móviles y la necesidad de mostrar fechas de manera legible para los usuarios, es importante saber cómo convertir una fecha en una cadena. Esto permite que la fecha se muestre de acuerdo a las preferencias de formato del usuario y puede ser utilizado en diferentes contextos, como en mensajes, notificaciones o en la interfaz de usuario.

## Cómo hacerlo

Para convertir una fecha en una cadena en Swift, puedes utilizar el tipo de dato `DateFormatter`, que se encarga de manejar la conversión de fechas en diferentes formatos. Primero, debes crear una instancia de `DateFormatter`, especificando el estilo de formato que deseas obtener:

```
let dateFormatter = DateFormatter()
dateFormatter.dateStyle = .long
```

Luego, puedes usar el método `string(from:)` para convertir una fecha en una cadena según el estilo de formato elegido:

```
let date = Date()
let dateString = dateFormatter.string(from: date) // "17 de diciembre de 2021"
```

Existen diferentes opciones de estilo de formato para personalizar aún más la cadena de salida. Por ejemplo, para mostrar la hora junto a la fecha, puede especificar el estilo de formato como `.full`:

```
dateFormatter.dateStyle = .full
dateFormatter.timeStyle = .full
```

```
let dateString = dateFormatter.string(from: date) // "viernes, 17 de diciembre de 2021 a las 14:30:00 hora central"
```

También puedes especificar un formato personalizado utilizando la propiedad `dateFormat` del `DateFormatter`. Por ejemplo:

```
dateFormatter.dateFormat = "dd/MM/yyyy"
let dateString = dateFormatter.string(from: date) // "17/12/2021"
```

## Profundizando

La conversión de fecha en una cadena no solo se limita a mostrarla en diferentes formatos. También puedes utilizar `DateFormatter` para convertir una cadena en una fecha. Para ello, debes especificar el formato de la cadena de entrada en la propiedad `dateFormat`:

```
dateFormatter.dateFormat = "yyyy-MM-dd"
let dateString = "2021-12-17"
let date = dateFormatter.date(from: dateString) // 17 de diciembre de 2021
```

Además, `DateFormatter` también cuenta con opciones para localizar la fecha y mostrarla en diferentes idiomas. Por ejemplo, puedes obtener la fecha en español utilizando el identificador de localización `es_ES`:

```
dateFormatter.locale = Locale(identifier: "es_ES")
let dateString = dateFormatter.string(from: date) // "viernes, 17 de diciembre de 2021"
```

## Ver también

- [Documentación de Apple sobre DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Uso de DateFormatter en una aplicación iOS con Swift](https://www.raywenderlich.com/7673530-dateformatter-tutorial-for-ios-getting-started)