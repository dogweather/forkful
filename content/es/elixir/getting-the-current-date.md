---
title:                "Obteniendo la fecha actual"
html_title:           "Elixir: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Por qué utilizar la versión actual de Elixir?

Si eres un programador en Elixir, sabrás que una de las ventajas de este lenguaje es su capacidad para manejar eficientemente las fechas y horas. Pero, ¿por qué sería importante obtener la fecha actual en primer lugar? Bueno, hay muchas aplicaciones en las que se requiere conocer la fecha y hora actual, como en un sistema de reservas o en una aplicación de seguimiento de tareas. En este artículo, te mostraremos cómo obtener la fecha actual en Elixir y te daremos un vistazo a cómo funciona bajo el capó.

## Cómo hacerlo

Para obtener la fecha actual en Elixir, utilizaremos el módulo `Calendar` y su función `local_time`. Esta función devuelve un struct con información detallada sobre la fecha y hora actual. Podemos almacenar esta estructura en una variable y acceder a sus valores individuales si es necesario.

```Elixir
current_date = Calendar.local_time
# %Calendar.DateTime{year: 2021, month: 10, day: 28, hour: 15, minute: 23, second: 45, microseconds: {720361, 6}, timezone: "UTC"}
```

También podemos utilizar la función `local_now` del módulo `NaiveDateTime`, que devuelve una fecha y hora en el formato naive datetime (sin información de zona horaria). Podemos convertir este formato a un datetime utilizando la función `DateTime.from_naive` del módulo `Calendar`.

```Elixir
naive_datetime = NaiveDateTime.local_now
# ~N[2021-10-28 15:30:21]
current_datetime = DateTime.from_naive(naive_datetime, "UTC")
# %DateTime{year: 2021, month: 10, day: 28, hour: 15, minute: 30, second: 21, timezone: "UTC"}
```

También es posible obtener solo la fecha actual utilizando la función `today` del módulo `Date`.

```Elixir
current_date = Date.today
# ~D[2021-10-28]
```

Y si solo necesitamos la hora actual, podemos utilizar la función `time_now` del módulo `Time`.

```Elixir
current_time = Time.time_now
# ~T[15:35:10.415530]
```

## Profundizando

Ahora que sabemos cómo obtener la fecha y hora actual en diferentes formatos, es interesante entender cómo funciona este proceso detrás de escena. Elixir utiliza la función `:port_command` para interactuar con el sistema operativo y obtener la fecha y hora actual. Esta función llama al comando `date` de UNIX y obtiene la fecha en el formato necesario.

Además, Elixir también utiliza el módulo `Erlang :calendar` para realizar operaciones de tiempo y fechas más complejas. Este módulo contiene funciones para manipular fechas y horas, así como también para consultar y comparar fechas y horas.

## Ver también

- [Documentación de Elixir sobre fechas y horas](https://hexdocs.pm/elixir/Calendar.html)
- [Ejemplos de uso del módulo Calendar](https://cultivatehq.com/posts/elixir-date-and-time/)
- [Ejemplos de funciones del módulo Date](https://elixirforum.com/t/how-to-get-current-date-in-elixir/45508/2)