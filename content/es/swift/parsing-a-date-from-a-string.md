---
title:                "Interpretando una fecha desde una cadena"
html_title:           "Swift: Interpretando una fecha desde una cadena"
simple_title:         "Interpretando una fecha desde una cadena"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué hacerlo?

La conversión de una fecha de texto a una instancia de fecha es un proceso común en la programación, donde convertimos una cadena de caracteres que contiene una fecha en un objeto de fecha que podemos manipular y mostrar de diferentes maneras. Los programadores a menudo hacen esto para facilitar el manejo de datos y para garantizar la precisión al trabajar con fechas.

## Cómo hacerlo:

En Swift, podemos utilizar el formateador de fechas (DateFormatter) para analizar una cadena de caracteres y convertirla en una fecha. Primero, definimos el formato de fecha utilizando los símbolos de formato de fecha y tiempo. Luego, utilizamos el método date(from:) del formateador de fechas para analizar la cadena y obtener una instancia de fecha.

```
// Definir el formato de fecha
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"

// Analizar la cadena y obtener una instancia de fecha
let dateStr = "31/12/2020"
let date = dateFormatter.date(from: dateStr)
print(date)

// Output: 2020-12-31 00:00:00 +0000
```

## Profundizando:

Antes de la introducción de los formateadores de fechas en Swift 3, los desarrolladores tenían que utilizar la clase NSCalendar para analizar una cadena de caracteres y convertirla en una fecha. Además, también existen bibliotecas de terceros que ofrecen funcionalidades más avanzadas para el análisis de fechas, como DateTools o SwiftDate.

## Ver también:

- Documentación oficial de Apple sobre el formateo de fechas en Swift: https://developer.apple.com/documentation/foundation/dateformatter
- Biblioteca DateTools para el análisis avanzado de fechas en Swift: https://github.com/MatthewYork/DateTools
- SwiftDate, una alternativa popular para el manejo de fechas en Swift: https://github.com/malcommac/SwiftDate