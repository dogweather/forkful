---
title:                "Haskell: Convirtiendo una fecha en una cadena de texto"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

El manejo de fechas y horas es una tarea común en la programación. Es importante poder convertir estas fechas en diferentes formatos, incluyendo cadenas de texto, para su uso en aplicaciones y sistemas. En este artículo, exploraremos cómo convertir una fecha en una cadena de texto utilizando Haskell.

## Cómo hacerlo

En Haskell, el módulo "Data.Time.Format" provee una función llamada "formatTime" que nos permite convertir una fecha en una cadena de texto utilizando un formato específico. Para utilizar esta función, primero debemos importar el módulo:

```Haskell
import Data.Time.Format
```

Luego, podemos utilizar la función "formatTime" de la siguiente manera:

```Haskell
formatTime defaultTimeLocale "%d/%m/%Y" (fromGregorian 2020 12 18)
```

En este ejemplo, estamos convirtiendo la fecha "18 de diciembre de 2020" en una cadena de texto con formato "dd/mm/yyyy" utilizando el tipo de dato "Gregorian" que proviene del módulo "Data.Time.Calendar". Esto imprimiría la cadena de texto "18/12/2020".

También podemos utilizar el mismo método para convertir la hora en una cadena de texto con formato "hh:mm:ss":

```Haskell
formatTime defaultTimeLocale "%H:%M:%S" (timeToTimeOfDay (UTCTime (fromGregorian 2020 12 18) (secondsToDiffTime 50340)))
```

En este ejemplo, estamos convirtiendo el tiempo "14:02:20" (50340 segundos) en una cadena de texto con formato "hh:mm:ss". Esto imprimiría la cadena de texto "14:02:20".

## Profundizando

La función "formatTime" puede aceptar diferentes formatos de cadena de texto, lo que nos permite personalizar aún más la salida. Algunos códigos de formato comunes utilizados en Haskell son:

- %Y: año completo (ej: 2020)
- %m: mes (ej: 12)
- %d: día del mes (ej: 18)
- %H: hora en formato 24 horas (ej: 14)
- %M: minutos (ej: 02)
- %S: segundos (ej: 20)

Podemos combinar estos códigos de formato para generar diferentes resultados, como por ejemplo:

- "%d/%m/%Y" -> 18/12/2020
- "%H:%M:%S" -> 14:02:20
- "%Y-%m-%d %H:%M:%S" -> 2020-12-18 14:02:20

También podemos utilizar la función "parseTimeM" del módulo "Data.Time.Format" para convertir una cadena de texto en una fecha o hora, utilizando el mismo formato utilizado en la función "formatTime". Esto es especialmente útil cuando queremos obtener una fecha de una base de datos o un archivo de texto.

## Ver también

- [Documentación de Data.Time.Format en Hackage](https://hackage.haskell.org/package/time/docs/Data-Time-Format.html)
- [Tutoriales de Haskell en español](https://happylearnhaskelltutorial.com/es/)