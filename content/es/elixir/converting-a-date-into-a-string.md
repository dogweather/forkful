---
title:                "Convertir una fecha en una cadena de caracteres"
html_title:           "Elixir: Convertir una fecha en una cadena de caracteres"
simple_title:         "Convertir una fecha en una cadena de caracteres"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Convertir un date en una cadena de caracteres puede ser una necesidad común en la programación. Ya sea para mostrar fechas en formato legible para el usuario o para realizar cálculos con fechas, esta conversión es esencial en muchos casos.

## Cómo hacerlo

```elixir
# Primero, importamos el módulo "DateTime" para poder trabajar con fechas y horas.
import DateTime

# Creamos una variable de fecha con el formato "año-mes-día".
fecha = ~D[2021-05-03]

# Utilizamos la función "to_string" para convertir la fecha en una cadena de caracteres.
DateTime.to_string(fecha)

# La salida será: "2021-05-03T00:00:00Z"
```

En este ejemplo, especificamos el formato de salida como "año-mes-día", pero es posible utilizar otros formatos como "mes/día/año" o "día-mes-año", entre otros.

Para obtener la hora actual en una cadena de caracteres, podemos utilizar la función `DateTime.utc_now/0`, la cual nos devuelve un objeto de fecha y hora que luego podemos convertir a una cadena de caracteres con la función `to_string`.

```elixir
# Obtenemos la hora actual en UTC y lo convertimos en una cadena de caracteres.
DateTime.utc_now() |> DateTime.to_string()

# La salida será algo como: "2021-05-03T11:30:00Z"
```

## Profundizando

La función `to_string` no solo nos permite especificar el formato de salida, sino también podemos especificar si queremos incluir la zona horaria o no. Por ejemplo, si queremos obtener la fecha y hora actual sin la zona horaria, podemos utilizar la opción `zone: false`:

```elixir
# Convertimos la hora actual en una cadena de caracteres sin la zona horaria.
DateTime.utc_now() |> DateTime.to_string(zone: false)

# La salida será algo como: "2021-05-03T11:30:00"
```

También es importante tener en cuenta que la función `to_string` solo funciona con objetos de fecha y hora del módulo `DateTime`. Si queremos convertir un objeto de fecha de otro módulo, como `Date`, debemos convertirlo primero a `DateTime`.

```elixir
# Creamos un objeto de fecha del módulo "Date".
fecha = Date.new(2021, 5, 3)

# Intentamos convertirlo directamente a una cadena de caracteres.
DateTime.to_string(fecha)

# Esto nos dará un error, ya que la función "to_string" solo funciona con objetos de "DateTime".

# Primero convertimos el objeto de fecha a "DateTime".
DateTime.from_date(fecha) |> DateTime.to_string()

# La salida será: "2021-05-03T00:00:00Z"
```

En resumen, convertir un date en una cadena de caracteres es una tarea sencilla en Elixir gracias a la función `to_string` del módulo `DateTime`. Es importante tener en cuenta el formato de salida y las opciones disponibles, así como también asegurarse de utilizar objetos de fecha del módulo correcto.

## Ver también

- Documentación oficial sobre la función `to_string` en el módulo `DateTime`: https://hexdocs.pm/elixir/DateTime.html#to_string/3
- Ejemplos de formatos de salida para fechas y horas: https://hexdocs.pm/elixir/DateTime.html#module-formatting