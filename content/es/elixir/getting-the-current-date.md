---
title:                "Elixir: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

# ¿Por qué necesitamos la fecha actual en Elixir?

La fecha y hora son elementos básicos en cualquier lenguaje de programación, y Elixir no es la excepción. La obtención de la fecha actual puede ser útil para una variedad de aplicaciones, como programar tareas, manejar comparaciones de tiempo o mostrar la fecha a los usuarios.

## Cómo obtener la fecha actual en Elixir

Elixir proporciona una función llamada `DateTime.now/0` que devuelve la fecha y hora actuales en un formato específico. Veamos un ejemplo:

```Elixir
iex> DateTime.now()
~N[2021-09-16 13:44:57.538813]
```

Como podemos ver, la respuesta es un `NaiveDateTime`, que no incluye información sobre la zona horaria. Si queremos incluir la zona horaria, podemos usar la función `DateTime.utc_now/0`:

```Elixir
iex> DateTime.utc_now()
~U[2021-09-16 17:44:41.249744Z]
```

Otra forma de obtener la fecha actual es utilizando `:os.system_time/0`, que devuelve el tiempo del sistema en segundos y microsegundos desde la época de Unix (1 de enero de 1970). Podemos convertir este valor en una fecha con la función `DateTime.from_unix/1`:

```Elixir
iex> DateTime.from_unix(:os.system_time())
{:ok, ~N[2021-09-16 14:10:06.405046]}
```

## Profundizando en la obtención de la fecha actual en Elixir

Si queremos obtener más información sobre la fecha actual, podemos utilizar el módulo `Calendar` de Elixir. Este módulo nos permite obtener la fecha, la hora y la zona horaria en diferentes formatos.

Veamos un ejemplo de cómo obtener la fecha actual en formato de un objeto `Date`:

```Elixir
iex> {year, month, day} = Calendar.date()
{2021, 9, 16}
```

También podemos obtener la hora actual en formato de un objeto `Time`:

```Elixir
iex> {hour, minute, second} = Calendar.time()
{14, 16, 47}
```

Además, podemos obtener información sobre la zona horaria actual utilizando `Calendar.time_zone/0` y `Calendar.time_zone_abbr/0`.

## Ver También

- [Documentación de Elixir sobre DateTime](https://hexdocs.pm/elixir/DateTime.html)
- [Documentación de Elixir sobre Calendar](https://hexdocs.pm/elixir/Calendar.html)
- [Página web de Elixir](https://elixir-lang.org/)

¡Ahora está listo para utilizar la fecha actual en sus aplicaciones en Elixir! ¡Buena suerte y feliz programación!