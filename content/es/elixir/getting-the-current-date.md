---
title:    "Elixir: Obteniendo la fecha actual"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado por qué es importante conocer la fecha actual al programar en Elixir? La respuesta es sencilla: la fecha y hora son elementos fundamentales en muchas aplicaciones, desde registrar eventos hasta realizar cálculos basados en el tiempo. En este artículo, aprenderás cómo obtener la fecha actual en tus proyectos de Elixir.

## Cómo hacerlo

Obtener la fecha actual en Elixir es muy sencillo gracias a la función `DateTime.utc_now()`. Esta función devolverá un objeto `DateTime` con la fecha y hora actuales en formato UTC. También puedes especificar una zona horaria específica utilizando la función `DateTime.now(zone)`, donde `zone` es un átomo que representa la zona horaria deseada, como `:utc` o `:local`.

```Elixir
current_date = DateTime.utc_now()
IO.inspect(current_date)

# Salida:
# %DateTime{calendar: Calendar.ISO, day: 15,
# hour: 22, microsecond: {18558, 3}, minute: 32, month: 9,
# second: 59, std_offset: 0, time_zone: "Etc/UTC",
# utc_offset: 0, year: 2021, zone_abbr: "UTC"}
```

También puedes obtener la fecha actual en un formato más legible utilizando la función `DateTime.to_iso8601(date)`. Esta función devolverá un string en formato ISO 8601, como `"2021-09-15T22:32:59.018558Z"`.

## Profundizando

La función `DateTime.utc_now()` puede parecer mágica, pero en realidad utiliza funciones básicas de Erlang para obtener la fecha y hora del sistema en el que se está ejecutando el código. Esto significa que es importante asegurarse de que la hora del sistema esté configurada correctamente para obtener resultados precisos.

También puedes personalizar el formato de salida utilizando la función `DateTime.to_string(date, format)`, donde `format` es una cadena de formato Erlang (no es necesario conocer el lenguaje para utilizar esta función). Por ejemplo, si quieres obtener la fecha actual en el formato "15/09/2021 22:32", puedes hacerlo utilizando `DateTime.to_string(current_date, "%d/%m/%Y %H:%M")`.

## Ver también

- [Documentación de DateTime](https://hexdocs.pm/elixir/DateTime.html)
- [Formato de fecha y hora en Elixir](https://elixirschool.com/es/lessons/specifics/date-time/)
- [Guía de formatos de fecha y hora de Erlang](http://erlang.org/doc/man/erlang.html#strftime-3)
- [Repositorio de Elixir en GitHub](https://github.com/elixir-lang/elixir)