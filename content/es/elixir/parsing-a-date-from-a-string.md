---
title:                "Analizando una fecha a partir de una cadena de texto"
html_title:           "Bash: Analizando una fecha a partir de una cadena de texto"
simple_title:         "Analizando una fecha a partir de una cadena de texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

El análisis de una fecha desde una cadena de texto consiste en convertir una representación de texto de una fecha y hora en un tipo de dato manejable por Elixir, como `Date`, `Time` o `DateTime`. Los programadores hacen esto para manipular, evaluar y usar las fechas de formas que no podrían si se mantuvieran como cadenas de texto.

## Cómo hacerlo:

Presentaremos un ejemplo simple de análisis de fecha utilizando la función `Date.from_iso8601/2` de Elixir. Para nuestro caso, comenzaremos con una cadena de texto que sigue el formato ISO 8601.

```elixir
fecha_str = "2021-11-23"
{:ok, fecha} = Date.from_iso8601(fecha_str)
IO.inspect(fecha)
```

La salida será:

```elixir
~D[2021-11-23]
```

Si nuestra cadena no sigue el formato correcto, recibiremos un error.

```elixir
fecha_str = "23-11-2021"
{:error, _} = Date.from_iso8601(fecha_str)
IO.inspect("Cadena con formato incorrecto")
```

Esto nos dará:

```elixir
"Cadena con formato incorrecto"
```

## Inmersión Profunda

Dentro de la biblioteca estándar de Elixir, `DateTime`, `Date` y `NaiveDateTime` proporcionan funciones para analizar fechas desde cadenas de texto. Este análisis basado en texto es crucial debido a la amplia utilización de la representación de texto para las fechas en las comunicaciones entre sistemas.

Alternativamente, si las fechas no siguen el formato ISO 8601, podemos considerar el uso de la biblioteca Timex que es muy flexible y maneja múltiples formatos de fechas.

La implementación específica de `from_iso8601/2` en Elixir implica el uso de expresiones regulares para analizar la cadena de texto y convertirla a una forma manipulable por Elixir. Estos detalless subyacentes están encapsulados dentro de la función y no necesitan ser entendidos para su uso diario.

## Ver También

- Documentación sobre `Date.from_iso8601/2`: [https://hexdocs.pm/elixir/Date.html#from_iso8601/2](https://hexdocs.pm/elixir/Date.html#from_iso8601/2)
- Documentación sobre `NaiveDateTime`: [https://hexdocs.pm/elixir/NaiveDateTime.html](https://hexdocs.pm/elixir/NaiveDateTime.html)
- Documentación sobre `DateTime`: [https://hexdocs.pm/elixir/DateTime.html](https://hexdocs.pm/elixir/DateTime.html)
- Biblioteca Timex en Hex: [https://hex.pm/packages/timex](https://hex.pm/packages/timex)