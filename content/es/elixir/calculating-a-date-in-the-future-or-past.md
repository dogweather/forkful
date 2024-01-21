---
title:                "Calcular una fecha en el futuro o pasado"
date:                  2024-01-20T17:30:42.877933-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcular una fecha en el futuro o pasado"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Calcular una fecha futura o pasada es simplemente determinar qué día será o fue después o antes de un tiempo específico. Los programadores lo hacen para tareas como programar eventos, recordatorios o verificar períodos de validez de tokens o sesiones.

## Cómo hacerlo:

Elixir facilita el trabajo con fechas usando la librería estándar y algunas adicionales como Timex. Aquí hay un ejemplo sencillo:

```elixir
# Asegúrate de tener Elixir y la librería Timex instalada
# Para instalar Timex, añádela a tu mix.exs: {:timex, "~> 3.7"}

# Código para calcular una fecha 30 días en el futuro:
{:ok, now} = DateTime.now("America/Mexico_City")
future_date = DateTime.add(now, 30 * 24 * 60 * 60)

IO.puts "Fecha actual: #{DateTime.to_string(now)}"
IO.puts "Fecha dentro de 30 días: #{DateTime.to_string(future_date)}"
```

Resultado de muestra:

```
Fecha actual: 2023-04-01T12:00:00Z
Fecha dentro de 30 días: 2023-05-01T12:00:00Z
```

## Deep Dive

En el pasado, los desarrolladores de Elixir usaban principalmente bibliotecas de terceros como Timex o Arrow para la manipulación de fechas debido a las limitaciones de la librería estándar. Sin embargo, desde la versión 1.3, Elixir ha mejorado la manipulación de fechas y horas con los módulos `DateTime`, `Date` y `Time`, haciendo que en muchos casos ya no sea necesario recurrir a dependencias externas.

Alternativas a `DateTime.add` incluyen funciones como `Date.add` que trabajan solo con fechas, sin información de la hora. También puedes restar días usando funciones negativas y hallar la diferencia entre dos fechas con `Date.diff`.

La implementación interna para el cálculo de fechas aprovecha las capacidades de Erlang, el lenguaje del sistema sobre el que se construye Elixir, garantizando precisión y fiabilidad en operaciones con fechas y horas.

## Ver También

Para profundizar más en el manejo de fechas en Elixir:

- [Documentación oficial de Elixir para DateTime](https://hexdocs.pm/elixir/DateTime.html)
- [Documentación oficial de Elixir para Date](https://hexdocs.pm/elixir/Date.html)
- [Documentación de la biblioteca Timex](https://hexdocs.pm/timex/Timex.html)
- [Elixir School: Lecciones sobre fechas y tiempos](https://elixirschool.com/es/lessons/basics/date-time/)