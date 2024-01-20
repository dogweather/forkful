---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

---
# Convertir una fecha a una cadena de texto con Haskell
El propósito de este artículo es presentarte cómo convertir una fecha en una cadena de texto o "string" en Haskell y explicarte por qué los programadores hacen esto.

## ¿Qué y por qué?
Convertir una fecha a una cadena de texto significa tomar la información de una fecha (típicamente en un formato tipo "Date" u "Object") y trasladarla a un format más legible para los humanos, como (dd-mm-aaaa) o (mm-dd-aaaa). Esta conversión se hace para facilitar la visualización y manipulación de la información relacionada con fechas.

## Cómo hacerlo:
Haskell proporciona diferentes soluciones para convertir una fecha a un string. Tomaremos `showGregorian` del módulo `Data.Time.Calendar` como ejemplo:

```Haskell
import Data.Time.Clock
import Data.Time.Calendar

main = do

 let day = fromGregorian 2022 2 28

 print (showGregorian day)
```

Después de ejecutar el programa, obtendrás `2022-02-28` como salida.

## Buceo profundo
Historicamente, la fecha y hora eran manipuladas con las bibliotecas `time` y `old-time` en Haskell. Más recientemente, el módulo `Data.Time` se expandió para proporcionar la funcionalidad de conversión de fecha-a-string. Aunque `showGregorian` es simple y fácil de usar, existen otras alternativas como `formatTime` que permiten una mayor personalización al mostrar la fecha. Por ejemplo, puedes especificar el formato de salida que deseas, como "dd-mm-aaaa" o "aaaa-mm-dd".

La implementación de `showGregorian` está basada en la división y módulo de enteros para separar los componentes de días, meses y años y convertirlos en cadenas.

## Ver también
Si deseas leer más sobre el manejo de fechas en Haskell, aquí algunas fuentes útiles:

1. [El módulo Data.Time en Hackage](http://hackage.haskell.org/package/time-1.11.1.1/docs/Data-Time.html).
3. [Información más avanzada sobre las fechas y el tiempo en Haskell](https://two-wrongs.com/haskell-time-library-tutorial).