---
aliases:
- /es/elixir/parsing-a-date-from-a-string/
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 02:05:05.416869-07:00
description: "Convertir una fecha de un string se trata de tomar texto, como \"2023-04-05\"\
  , y convertirlo a un formato de fecha que tu programa pueda entender y usar.\u2026"
lastmod: 2024-02-18 23:09:09.658318
model: gpt-4-0125-preview
summary: "Convertir una fecha de un string se trata de tomar texto, como \"2023-04-05\"\
  , y convertirlo a un formato de fecha que tu programa pueda entender y usar.\u2026"
title: Analizando una fecha a partir de una cadena de texto
---

{{< edit_this_page >}}

## Qué y Por Qué?

Convertir una fecha de un string se trata de tomar texto, como "2023-04-05", y convertirlo a un formato de fecha que tu programa pueda entender y usar. Los programadores hacen esto porque las fechas vienen en muchos formatos, y necesitan consistencia para compararlas, ordenarlas o almacenarlas adecuadamente.

## Cómo hacerlo:

En Elixir, puedes analizar fechas usando el módulo `Date`. Así se convierte un string en una fecha:

```elixir
date_string = "2023-04-05"
{:ok, date} = Date.from_iso8601(date_string)
IO.inspect(date)
```

Salida de muestra:

```elixir
~D[2023-04-05]
```

Para manejar diferentes formatos, puedes usar la biblioteca `Timex`:

```elixir
{:ok, datetime} = Timex.parse("05-04-2023", "{D}-{0M}-{YYYY}")
IO.inspect(datetime)
```

Salida de muestra:

```elixir
#DateTime<2023-04-05 00:00:00Z>
```

## Análisis Profundo

La función `Date.from_iso8601/1` es parte de la biblioteca estándar de Elixir, introducida para asegurar un análisis fácil del estándar de fecha ISO8601 - un formato de fecha común. Pero la vida no es tan simple; las fechas vienen en toneladas de formatos. Ahí es donde `Timex`, una biblioteca de terceros de Elixir, entra en juego. Es más rica que las funciones de fecha integradas en Elixir y ayuda a manejar una amplia variedad de formatos de fecha.

Elixir en sí es inmutable, lo que significa que las fechas analizadas no son una excepción; no pueden cambiarse una vez creadas. Esta característica se remonta a las raíces de programación funcional de Elixir, garantizando predictibilidad y facilitando la depuración.

Históricamente, el análisis de fechas ha sido difícil debido a los estándares variados. Sin embargo, con bibliotecas como `Timex` y características del lenguaje en Elixir, la complejidad se abstrae, haciendo la vida de un desarrollador un poco más sencilla.

## Ver También

- [Fecha de Elixir](https://hexdocs.pm/elixir/Date.html) (en inglés)
- [Documentación de Timex](https://hexdocs.pm/timex/Timex.html) (en inglés)
- [Estándar ISO8601](https://www.iso.org/iso-8601-date-and-time-format.html) (en inglés)
