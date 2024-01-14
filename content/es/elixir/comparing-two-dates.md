---
title:                "Elixir: Comparando dos fechas"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué 

Comparar fechas es una tarea común en la programación, especialmente en aplicaciones web y sistemas de gestión de bases de datos. Al comprender cómo comparar de manera efectiva dos fechas, podemos asegurarnos de que nuestras aplicaciones funcionen de manera eficiente y sin bugs.

## Cómo hacerlo

En Elixir, podemos comparar dos fechas utilizando el operador `==`, que verifica si ambos valores son iguales. Sin embargo, esto no siempre es suficiente, ya que a menudo queremos tener en cuenta factores como la zona horaria y el formato de fecha. Veamos algunos ejemplos utilizando el módulo `Date` de la biblioteca estándar de Elixir. 

```Elixir
iex> date_1 = Date.from_iso8601("2021-05-25")
{:ok, ~D[2021-05-25]}

iex> date_2 = Date.from_iso8601("2021-05-25")
{:ok, ~D[2021-05-25]}

iex> date_1 == date_2
true
```

En este ejemplo, hemos creado dos fechas iguales y al compararlas obtenemos como resultado `true`. Pero, ¿qué sucede si queremos comparar la fecha actual con una fecha en el futuro? Para eso, podemos utilizar la función `compare/2` que devuelve uno de los siguientes tres átomos: `:lt (menor que)`, `:gt (mayor que)` o `:eq (igual a)`.

```Elixir
iex> today = Date.utc_today()
{:ok, ~D[2021-11-05]}

iex> future_date = ~D[2022-02-14]
~D[2022-02-14]

iex> Date.compare(today, future_date)
:lt 
```

También podemos utilizar la función `diff/2` para obtener la diferencia en días entre dos fechas.

```Elixir
iex> Date.diff(~D[2021-01-01], ~D[2022-01-01])
365
```

## Profundizando 

Comparar fechas puede ser complicado cuando se tienen en cuenta diferentes zonas horarias y formatos. Por ejemplo, si queremos comparar dos fechas con diferentes zonas horarias, debemos ajustar la fecha a la misma zona horaria antes de realizar la comparación. 

```Elixir
iex> date_1 = Date.new(2021, 5, 25, "Europe/Madrid") 
~D[2021-05-25]

iex> date_2 = Date.new(2021, 5, 25, "America/New_York")
~D[2021-05-25]

iex> date_1 == date_2 
true 
```

También es importante asegurarse de que las fechas estén en el mismo formato antes de compararlas. Podemos utilizar la función `from_europeans/1` para convertir una fecha en formato europeo al formato americano. 

## Ver también 

- Documentación oficial de Elixir: https://elixir-lang.org/getting-started/typespecs-and-behaviours.html#date 
- Artículo sobre comparación de fechas en Elixir: https://medium.com/@chathuranga94/how-to-compare-dates-and-timestamps-in-elixir-9df7da123c57 
- Biblioteca de fechas y tiempos para Elixir: https://hexdocs.pm/timex/Timex.html