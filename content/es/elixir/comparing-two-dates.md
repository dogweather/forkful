---
title:                "Elixir: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué
Comparar dos fechas es una tarea común en la programación, ya sea para verificar si una fecha es posterior a otra o para calcular la diferencia entre dos fechas. En este artículo, exploraremos cómo realizar esta tarea en Elixir.

## Cómo hacerlo
Para comparar dos fechas en Elixir, podemos utilizar la función `DateTime.compare/2`. Esta función toma dos fechas como argumentos y devuelve un valor que indica si la primera fecha es anterior, igual o posterior a la segunda fecha.

Veamos un ejemplo de cómo utilizar esta función en código:

```elixir
fecha_1 = DateTime.from_naive!(~N[2020-01-01 09:00:00], "America/Mexico_City")
fecha_2 = DateTime.from_naive!(~N[2020-02-01 09:00:00], "America/Mexico_City")

if DateTime.compare(fecha_1, fecha_2) == :lt do
  IO.puts "La fecha 1 es anterior a la fecha 2."
elsif DateTime.compare(fecha_1, fecha_2) == :eq do
  IO.puts "Las fechas son iguales."
else
  IO.puts "La fecha 2 es anterior a la fecha 1."
end
```

En este ejemplo, hemos creado dos fechas y las hemos comparado utilizando la función `DateTime.compare/2`. En este caso, la salida del código sería:

```
La fecha 1 es anterior a la fecha 2.
```

## Deep Dive
Elixir utiliza una representación interna de las fechas llamada [Erlang calendar](https://erlang.org/doc/man/calendar.html). Esto significa que podemos utilizar las funciones de calendario de Erlang para trabajar con fechas en Elixir.

Además de la función `DateTime.compare/2`, también podemos utilizar otras funciones como `DateTime.diff/2` para calcular la diferencia entre dos fechas y `DateTime.to_erl/1` para convertir una fecha de Elixir a su equivalente en Erlang.

Es importante tener en cuenta que las fechas en Elixir son inmutables, lo que significa que no se pueden modificar una vez creadas. Por lo tanto, al utilizar funciones como `DateTime.add/2` o `DateTime.sub/2` para sumar o restar días a una fecha, se devuelve una nueva fecha en lugar de modificar la fecha original.

## Ver también
- [Documentación de Elixir sobre fechas y horas](https://hexdocs.pm/elixir/DateTime.html)
- [Funciones de calendario de Erlang](https://erlang.org/doc/apps/stdlib/calendar.html)