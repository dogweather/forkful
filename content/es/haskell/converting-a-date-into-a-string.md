---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "Haskell: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Convertir una fecha en una cadena de texto es el proceso de transformar una fecha en un formato legible para los humanos, como "12 de marzo de 2021". Los programadores a menudo realizan esta conversión para mostrar fechas en interfaces de usuario o para almacenarlas en una base de datos.

## Cómo:

Aquí hay un ejemplo simple en Haskell de cómo convertir una fecha en una cadena de texto:

```Haskell
import Data.Time.Format
import Data.Time.Clock
import System.Locale

main = do
  let date = UTCTime (fromGregorian 2021 3 12) 0
      format = "%d de %B de %Y"
  putStrLn $ formatTime defaultTimeLocale format date
```
 Esto producirá la siguiente salida:

`12 de marzo de 2021`

## Inmersión profunda:

La conversión de fechas en cadenas de texto ha sido un problema común en programación desde los primeros días de la informática. En el pasado, los programadores tenían que escribir su propio código para convertir fechas en cadenas de texto, lo que a menudo era tedioso y propenso a errores. Sin embargo, con la disponibilidad de bibliotecas y funciones de alto nivel en lenguajes modernos como Haskell, esta tarea es mucho más sencilla y menos propensa a errores.

Un enfoque alternativo para convertir fechas en cadenas de texto es utilizar bibliotecas externas, como `time-locale-compat` y `time-format`. Estas bibliotecas proporcionan funciones adicionales y opciones de formato para la conversión de fechas.

En términos de implementación, la mayoría de las bibliotecas de Haskell utilizan el formato de fecha y hora ISO 8601, que es ampliamente reconocido y utilizado en todo el mundo.

## Ver también:

- Documentación sobre formatTime en la [Hackage de Haskell](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html#v:formatTime).
- Detalles sobre el [formato ISO 8601](https://es.wikipedia.org/wiki/ISO_8601) para fechas y horas.