---
title:                "Analizando una fecha de una cadena"
html_title:           "Elm: Analizando una fecha de una cadena"
simple_title:         "Analizando una fecha de una cadena"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Parsear una fecha de una cadena de texto es el proceso de extraer información específica de una fecha, como el día, mes y año, a partir de una cadena de texto que la contiene en un formato específico. Los programadores hacen esto para manipular y utilizar la información de la fecha en sus programas, como en la creación de calendarios o en el cálculo de la duración entre dos fechas.

## Cómo:

```Elm
import Date exposing (fromIsoString)

Date.fromString "2021-09-30"
```

Este ejemplo utiliza la función `fromIsoString` del módulo `Date` para convertir la cadena de texto "2021-09-30" en un valor de fecha de Elm en formato ISO. El resultado sería `Ok (Date.fromString "2021-09-30")`, lo que significa que la fecha fue parseada con éxito y se puede utilizar en el programa.

## Profundizando:

### Contexto Histórico:

Antes de la introducción del estándar de formato ISO para fechas, los programadores debían lidiar con una variedad de formatos de fecha en diferentes regiones y sistemas operativos. La necesidad de parsear una fecha de una cadena de texto surgió para unificar y facilitar el manejo de esta información.

### Alternativas:

Aunque Elm ofrece la función `Date.fromString` para parsear fechas, existen otras herramientas y bibliotecas que pueden ser útiles dependiendo del contexto del programa, como la biblioteca `elm-community/date-extra` que ofrece funciones adicionales para manipular y formatear fechas.

### Detalles de Implementación:

La función `Date.fromString` utiliza el estándar de formato ISO para parsear la fecha. Esto significa que la cadena de texto debe seguir un formato específico: "YYYY-MM-DD". Si la cadena de texto no se ajusta a este formato, se lanzará un error.

## También puedes ver:

- [Documentación de Elm sobre manejo de fechas](https://package.elm-lang.org/packages/elm/time/latest/)
- [Tutorial sobre manejo de fechas en Elm](https://guide.elm-lang.org/architecture/effects/time.html)
- [Biblioteca extra de fechas para Elm](https://github.com/elm-community/date-extra)