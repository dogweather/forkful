---
title:                "Extracción de una fecha de una cadena"
html_title:           "Gleam: Extracción de una fecha de una cadena"
simple_title:         "Extracción de una fecha de una cadena"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

En pocas palabras, analizar una fecha de una cadena de texto significa tomar una fecha escrita en un formato determinado (como DD/MM/AAAA) y convertirla en una estructura de datos que un programa pueda manipular y utilizar. Los programadores realizan esta tarea con el fin de comprender y manejar los datos de manera más efectiva en sus aplicaciones.

## Cómo hacerlo:

El siguiente código muestra cómo analizar una fecha de una cadena de texto en Gleam:

```Gleam
// Importar módulo de fecha y cadena
import gleam/time
import gleam/string

// Definir una cadena de texto con una fecha en formato DD/MM/AAAA
let date_string = "31/12/2021"

// Analizar la fecha y guardar el resultado en una variable
let parsed_date = String.to_date(date_string, "%d/%m/%Y")

// Imprimir la fecha analizada en el formato deseado
let desired_format = "%B %d, %Y" //Ejemplo: "December 31, 2021"
let formatted_date = Date.format(desired_format, parsed_date)
IO.print(formatted_date) //Salida: "December 31, 2021"
```

## Inmersión profunda:

Analizar una fecha de una cadena de texto puede ser una tarea complicada ya que las fechas se pueden escribir en una variedad de formatos y pueden variar entre diferentes regiones y culturas. Además, existen alternativas a Gleam para realizar esta tarea, como paquetes de manejo de fechas en otros lenguajes de programación. Sin embargo, Gleam ofrece una solución sólida y eficiente con su módulo de fecha y cadena incorporado. Este módulo también incluye funciones para manejar zonas horarias y realizar cálculos con fechas.

## Ver también:

- Documentación del módulo de fecha y cadena en [la página oficial de Gleam](https://gleam.run/documentation/standard-library/time/)
- Ejemplos y ejercicios para practicar el análisis de fechas en [este artículo de Rosetta Code](https://rosettacode.org/wiki/Determine_if_a_string_is_date)