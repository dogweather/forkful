---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Convertir Fechas a Strings en Swift

## ¿Qué y Por Qué?
Transformar una fecha en string significa representar una fecha como texto. Los programadores solemos hacerlo para facilitar la visualización y manipulación de fechas en nuestras aplicaciones.

## Cómo Hacerlo:
En Swift, la clase `DateFormatter` te permite convertir una Fecha en string, formateándola como desees. Aquí está el código:

```Swift
import Foundation

let date = Date()
let formatter = DateFormatter()
  
// Aquí escoges el formato de la fecha
formatter.dateFormat = "yyyy-MM-dd"

let result = formatter.string(from: date)
print(result)
```
Ejecutando este código, obtendrías un resultado parecido a este:

```BASH
2022-03-15
```

## Profundización:

### 1. Contexto Histórico
La representación de fechas como strings se ha vuelto común con la evolución de la programación, debido a la necesidad de visualizar datos de fecha de manera comprensible.

### 2. Alternativas
Swift proporciona varios formatos predefinidos (como `short`, `medium`, `long` y `full`), que pueden ser útiles cuando no necesitas un formato de fecha personalizado:
```Swift
formatter.dateStyle = .medium
```

### 3. Detalles de Implementación
Cuando conviertes una fecha en string, ten en cuenta las diferencias locales en el formato de fechas. Usa el `locale` de `DateFormatter` para adecuar el formato al idioma y las convenciones del usuario:

```Swift
formatter.locale = Locale(identifier: "es_ES") // Formato de Fecha Español (Spain)
```

## Ver También
Puedes aprender más acerca de `DateFormatter` y el manejo de fechas en Swift en la documentación oficial [aquí](https://developer.apple.com/documentation/foundation/dateformatter).
Para más tutoriales sobre Swift y codificación, visita [Swift por Sundell](https://www.swiftbysundell.com/).

No hay dudas, convertir fechas en strings es una práctica esencial en Swift. ¿Estás listo para ponerlo en práctica?