---
title:    "Elixir: Convirtiendo una fecha en una cadena de caracteres"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## ¿Por qué convertir una fecha en una cadena?

La conversión de una fecha en una cadena es una tarea común en la programación. Puede ser útil en varias situaciones, como mostrar la fecha en una interfaz de usuario, guardarla en una base de datos o enviarla en una solicitud HTTP.

## ¿Cómo hacerlo?

Para convertir una fecha en una cadena, podemos utilizar la función `DateTime.to_iso8601` de la biblioteca estándar de Elixir. Esta función acepta un objeto `DateTime` y devuelve una cadena en formato ISO 8601.

```Elixir
date = DateTime.utc_now()
# %DateTime{calendar: Calendar.ISO, day: 12, hour: 17, microsecond: {637995, 6}, minute: 36, month: 11, second: 6, std_offset: 0, time_zone: "Etc/UTC", year: 2021}
DateTime.to_iso8601(date)
# "2021-11-12T17:36:06Z"
```
Podemos ver que la cadena resultante incluye la fecha y hora en formato de 24 horas, junto con la zona horaria UTC.

Otra opción es utilizar la función `DateTime.to_string`, que permite especificar un formato personalizado para la cadena resultante.

```Elixir
date = DateTime.utc_now()
# %DateTime{calendar: Calendar.ISO, day: 12, hour: 17, microsecond: {318811, 6}, minute: 43, month: 11, second: 57, std_offset: 0, time_zone: "Etc/UTC", year: 2021}
DateTime.to_string(date, "{YYYY}/{M}/{D} a las {h}:{m} {a} ({z})", :strftime)
# "2021/11/12 a las 5:43 PM (UTC)"
```

Podemos especificar diferentes formatos dependiendo de nuestras necesidades. Por ejemplo, para obtener solo la fecha en formato "DD/MM/YYYY", podemos utilizar `"{D}/{M}/{YYYY}"`.

## Profundizando en la conversión de una fecha a una cadena

Elixir utiliza el módulo `Calendar` para manejar las fechas y horas. Esto significa que podemos utilizar diferentes calendarios y zonas horarias en nuestras conversiones.

Por ejemplo, utilizando el calendario gregoriano y la zona horaria Colombia (GMT-5), tenemos:

```Elixir
date = DateTime.from_naive({2021, 11, 12}, "America/Bogota")
# %DateTime{calendar: Calendar.Gregorian, day: 12, hour: 0, microsecond: {0, 0}, minute: 0, month: 11, second: 0, std_offset: -18000, time_zone: "America/Bogota", year: 2021}
DateTime.to_string(date, "{D}/{M}/{YYYY} a las {h}:{m} {a} ({z})", :strftime)
# "12/11/2021 a las 12:00 AM (GMT-5)"
```

Vemos que la zona horaria se incluye en la cadena resultante. También podemos convertir la fecha a diferentes calendarios, utilizando la función `DateTime.to_date` y especificando un calendario específico.

```Elixir
date = DateTime.utc_now()
# %DateTime{calendar: Calendar.ISO, day: 12, hour: 19, microsecond: {591718, 6}, minute: 52, month: 11, second: 12, std_offset: 0, time_zone: "Etc/UTC", year: 2021}
DateTime.to_date(date, Calendar.Julian)
# %Date{calendar: Calendar.Julian, day: 30, month: 10, year: 2021}
```

## Ver también

- [Documentación oficial de Elixir en la conversión de fechas y horas](https://hexdocs.pm/elixir/DateTime.html#to_string/3)
- [Explicación detallada de ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html)
- [Lista de calendarios y zonas horarias disponibles en Elixir](https://hexdocs.pm/elixir/Calendar.html)