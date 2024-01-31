---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:16:45.822281-07:00
html_title:           "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"

category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Obtener la fecha actual en Swift significa capturar el momento presente en tu código. Los programadores hacen esto para registrar eventos, comparar fechas o mostrar información en tiempo real.

## Cómo Hacerlo:
Swift hace que obtener la fecha actual sea fácil con la clase `Date`. Aquí tienes un ejemplo directo:

```Swift
import Foundation

let fechaActual = Date()

print(fechaActual)
```

El output será similar a:

```
2023-04-05 13:00:00 +0000
```

## Análisis Detallado:
La clase `Date` en Swift viene del Foundation Framework y maneja las fechas y horas. Antes de Swift, Objective-C usaba `NSDate`, pero Swift proporciona una API más limpia y moderna.

Hay alternativas, por ejemplo, `Calendar` y `DateFormatter`, para cuando necesitas más control o presentar las fechas en diferentes formatos.

Detalles de implementación:
- `Date()` crea una instancia representando el momento exacto.
- Swift gestiona las zonas horarias automáticamente, pero siempre puedes especificar una si es necesario.
- Es bueno saber que `Date` usa la hora UTC y que para mostrar la hora local, necesitarás un `DateFormatter`.

## Ver También:
- [Documentación de Apple sobre la clase Date](https://developer.apple.com/documentation/foundation/date)
- [Tutorial sobre cómo usar DateFormatter](https://www.hackingwithswift.com/articles/140/the-complete-guide-to-dates-and-times-in-swift)
