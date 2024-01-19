---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Consiguiendo la Fecha Actual en Elixir

## ¿Qué y Por Qué?

Conseguir la fecha actual en Elixir nos permite trabajar con información en tiempo real. Es esencial para operaciones como llevar un registro de cuándo se realizó una determinada acción o calcular el tiempo transcurrido.

## ¿Cómo hacerlo?
Para conseguir la fecha actual en Elixir, usamos el módulo `DateTime` proporcionado por Elixir.

```elixir
fecha_actual = DateTime.utc_now() 

IO.inspect fecha_actual
```

Al ejecutar este código, obtendrías un output como este:

```elixir
~U[2022-05-11T11:54:25.456Z]
```
Se puede apreciar que `DateTime.utc_now()` devuelve la fecha y hora actual en formato `DateTime` UTC. 

## Inmersión Profunda

La necesidad de trabajar con fechas ha sido parte de la programación desde siempre. Elixir, una lengua funcional moderna, simplifica este proceso con el módulo `DateTime`, introducido en su versión 1.3.

Existen alternativas como la librería de terceros `Timex`, que proporciona más funciones que `DateTime`. Sin embargo, para conseguir simplemente la fecha actual, `DateTime` es suficiente.

Internamente, `DateTime.utc_now()` utiliza la función `:os.system_time()` de Erlang, lo que significa que la fecha y hora que devuelve son tan precisas como lo permita el sistema operativo del servidor.

## Ver También

Si quieres profundizar en estas cuestiones, aquí tienes algunas fuentes relacionadas:

- La [documentación oficial de Elixir](https://hexdocs.pm/elixir/DateTime.html) sobre `DateTime`.
- La [documentación sobre el módulo `:os` de Erlang](http://erlang.org/doc/man/os.html).
- La [biblioteca `Timex`](https://hexdocs.pm/timex/readme.html) si necesitas más funcionalidades.