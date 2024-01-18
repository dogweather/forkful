---
title:                "Analizando una fecha de una cadena"
html_title:           "Elixir: Analizando una fecha de una cadena"
simple_title:         "Analizando una fecha de una cadena"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

### ¿Qué y por qué?
El análisis de una fecha a partir de una cadena de texto es el proceso de convertir una fecha escrita en formato de texto en un objeto de fecha válido en un lenguaje de programación. Los programadores a menudo lo hacen para poder utilizar y manipular fechas en sus aplicaciones.

### Cómo hacerlo:
Vamos a ver cómo podemos analizar una fecha a partir de una cadena de texto en Elixir. Primero, necesitamos la librería `Calendar` para trabajar con fechas. Luego, utilizaremos la función `Date.from_iso8601/1` para pasarle una cadena de texto en formato ISO 8601 y obtener una fecha en formato Elixir.

```
Elixir Calendar
Date.from_iso8601("2021-06-15")
```

Esto nos devolverá un objeto de fecha en formato Elixir que podemos utilizar en nuestra aplicación.

### Inmersión profunda:
El análisis de fechas de cadenas de texto es una tarea común en programación. Antes de la introducción de la librería `Calendar` en Elixir, los programadores tenían que usar otras librerías o escribir su propio código para analizar fechas. Sin embargo, al utilizar la función `Date.from_iso8601/1` en Elixir, podemos obtener una fecha válida de manera fácil y sencilla.

Si bien la función `Date.from_iso8601/1` es útil para analizar fechas en formato ISO 8601, existen otras librerías y funciones en Elixir que pueden ser útiles para otros formatos de fecha, como `Date.from_iso8601!/1` para analizar fechas sin manejo de errores o `Date.from_erl!/1` para convertir fechas de Erlang.

### Véase también:
- [Documentación de Elixir: Fechas y Horas](https://hexdocs.pm/elixir/1.12/Calendar.html)
- [Librería de Elixir para análisis de fechas: Timex](https://hex.pm/packages/timex)
- [Repositorio de GitHub de Elixir: Ejemplos de análisis de fechas](https://github.com/elixir-lang/elixir)