---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "Elixir: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Convertir una fecha en una cadena de texto es un proceso común en la programación, especialmente cuando se trabaja con fechas y horarios en un sistema. Esto se hace para que la fecha se pueda mostrar de forma legible para los usuarios. Los programadores lo hacen para facilitar la visualización y manipulación de las fechas en sus aplicaciones.

## Cómo hacerlo:

```Elixir  
  date = ~D[2021-11-01]
  String.to_charlist(date) 
```

Salida esperada: "2021-11-01"

El ejemplo anterior muestra cómo se puede convertir una fecha en una cadena de texto en Elixir. Primero, se define la fecha en el formato de fecha Elixir usando el prefijo `~D` seguido de la fecha entre corchetes. Luego, se utiliza la función `String.to_charlist/1` para convertir la fecha en una lista de caracteres, que es lo mismo que una cadena de texto.

## Inmersión profunda:

Con convertir una fecha en una cadena de texto, uno puede obtener elementos específicos, como el día de la semana, o manipular la fecha según sus necesidades. Antes, en Elixir, se usaba la función `DateTime.to_string/3` para realizar esta tarea. Sin embargo, a partir de Elixir 1.5, se recomienda usar la función `String.to_charlist/1` como se mostró en el ejemplo anterior.

## Vea también:

- Documentación oficial de Elixir String.to_charlist/1 (https://hexdocs.pm/elixir/String.html)
- Tutorial de conversión de fechas en Elixir (https://elixirschool.com/en/lessons/basics/strings/#date-conversion)
- StackOverflow para preguntas de programación (https://stackoverflow.com/questions/tagged/elixir)