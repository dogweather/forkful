---
title:                "Analizando una fecha de una cadena"
html_title:           "Haskell: Analizando una fecha de una cadena"
simple_title:         "Analizando una fecha de una cadena"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Parsing de una fecha de una cadena es un proceso en el que se convierte una cadena de caracteres que representa una fecha en un formato específico, en un tipo de dato que el programa pueda utilizar para manipular y realizar operaciones sobre esa fecha. Los programadores realizan esta tarea para poder trabajar con fechas de manera más eficiente y precisa en sus aplicaciones.

## ¡Manos a la obra!

```Haskell
-- Importando el módulo Data.Time para utilizar funciones de parsing de fechas
import Data.Time

-- Parseando una fecha en formato ISO8601
parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" "2022-01-01T12:00:00" :: Maybe UTCTime
-- Devuelve: Just 2022-01-01 12:00:00 UTC

-- Parseando una fecha en formato personalizado
parseTimeM True defaultTimeLocale "%A, %d de %B de %Y" "Miércoles, 01 de Enero de 2022" :: Maybe Day
-- Devuelve: Just 2022-01-01
```

## Profundizando

Parsear fechas se ha vuelto una tarea cada vez más común en la programación moderna, ya que permite manipular fechas de manera más sencilla y precisa. Antes de que existieran funciones de parsing, los programadores debían realizar complejos cálculos para manipular fechas en sus aplicaciones. Actualmente, existen diferentes alternativas y librerías en otros lenguajes de programación para realizar esta tarea, como Moment.js en JavaScript o la librería DateTime en Python. En Haskell, podemos utilizar la función "parseTimeM" del módulo Data.Time para realizar el parsing de fechas de manera efectiva y sencilla.

## Más información

- [Hackage: Data.Time](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Wikipedia: ISO 8601](https://es.wikipedia.org/wiki/ISO_8601)
- [Moment.js](https://momentjs.com/)
- [Python DateTime](https://docs.python.org/3.9/library/datetime.html)