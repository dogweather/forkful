---
title:                "Comparando dos fechas"
html_title:           "C#: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Comparar dos fechas permite a los programadores determinar qué fecha es anterior o posterior. Esto es crucial en muchas aplicaciones, como las que manejan reservaciones, pasajes de avión, gestión de eventos, etc.

## ¿Cómo Hacerlo?

Primero, definamos dos fechas en Elixir:

```elixir
fecha1 = ~D[2022-12-31]
fecha2 = ~D[2023-01-01]
```

Luego, podemos usar la función `Date.compare/2` para comparar estas dos fechas:

```elixir
comparacion = Date.compare(fecha1, fecha2)
IO.puts comparacion
```

Eso mostrará `:lt` en la salida, que significa que la `fecha1` es menos que la `fecha2`.

## Inmersión Profunda

Históricamente, comparar fechas era más complicado debido a la falta de funciones incorporadas en los lenguajes de programación, pero ahora es sencillo gracias a Elixir.

Si `Date.compare/2` no te conviene, puedes extraer los campos del año, mes y día de cada fecha y compararlos uno por uno. Esto, sin embargo, puede ser más tedioso y propenso a errores.

La implementación de `Date.compare/2` en el código fuente de Elixir es interesante: básicamente utiliza el módulo de comparación Erlang para comparar dos tuplas de año, mes y día.

## Ver También

Explora los siguientes enlaces para más detalles y ejemplos:

- Documentación oficial de Elixir sobre el módulo Date: https://hexdocs.pm/elixir/Date.html
- Código fuente de Elixir donde podrías aprender sobre la implementación de `Date.compare/2`: https://github.com/elixir-lang/elixir/blob/v1.12/lib/elixir/lib/calendar/date.ex#L270