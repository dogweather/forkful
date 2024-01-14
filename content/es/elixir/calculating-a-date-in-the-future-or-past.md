---
title:                "Elixir: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías calcular una fecha en el futuro o en el pasado?

Existe una gran variedad de razones por las que puedes necesitar calcular una fecha en el futuro o en el pasado en tu programa de Elixir. Tal vez necesites realizar un seguimiento de una fecha de vencimiento, o quizás necesites planificar eventos con antelación. Sea cual sea el caso, el cálculo de fechas puede ser una tarea complicada, pero en Elixir, es más sencillo de lo que piensas. ¡Sigue leyendo para aprender cómo hacerlo!

## Cómo calcular una fecha en el futuro o en el pasado en Elixir

El módulo `DateTime` de Elixir contiene varias funciones útiles para calcular y manipular fechas. Para calcular una fecha en el futuro, podemos usar la función `add/3` que toma tres argumentos: una fecha, un número de días y una unidad de tiempo (días, meses o años).

```Elixir
iex> date = ~D[2021-10-13] # definimos la fecha de partida
~D[2021-10-13]
iex> DateTime.add(date, 3, :days) # sumamos 3 días a la fecha
~D[2021-10-16]
iex> DateTime.add(date, 2, :months) # sumamos 2 meses a la fecha
~D[2021-12-13]
iex> DateTime.add(date, 1, :years) # sumamos 1 año a la fecha
~D[2022-10-13]
```

Para calcular una fecha en el pasado, también podemos utilizar la función `add/3`, pero en este caso le pasamos un número negativo como segundo argumento.

```Elixir
iex> date = ~D[2021-10-13] # definimos la fecha de partida
~D[2021-10-13]
iex> DateTime.add(date, -5, :days) # restamos 5 días a la fecha
~D[2021-10-08]
iex> DateTime.add(date, -1, :months) # restamos 1 mes a la fecha
~D[2021-09-13]
iex> DateTime.add(date, -2, :years) # restamos 2 años a la fecha
~D[2019-10-13]
```

Puedes jugar con diferentes valores para los argumentos y probar distintas unidades de tiempo para ver cómo afecta al resultado. También puedes utilizar otras funciones del módulo `DateTime` para manipular las fechas de diferentes maneras.

## Profundizando en el cálculo de fechas en Elixir

Ahora que sabes cómo calcular una fecha en el futuro o en el pasado, puede que te preguntes qué tipo de cálculos más complejos puedes hacer con fechas en Elixir. Una de las posibilidades es determinar el día de la semana en el que cae una fecha específica. Para hacerlo, podemos utilizar la función `day_of_week/1` del módulo `DateTime`, que devuelve un número del 1 al 7 correspondiente al día de la semana (lunes a domingo).

```Elixir
iex> date = ~D[2021-10-13]
~D[2021-10-13]
iex> DateTime.day_of_week(date) # nos devuelve el día de la semana (3 es miércoles)
3
```

Esto puede ser especialmente útil si, por ejemplo, queremos programar una tarea para que se ejecute solo en ciertos días de la semana.

## Mira también

¡Ahora ya sabes cómo calcular fechas en el futuro o en el pasado en Elixir! Si quieres aprender más sobre el manejo de fechas y tiempos en Elixir, puedes consultar la documentación oficial, [este blog post](https://learningelixir.jot.com/WikiHome/ElixirTricks) y [este tutorial](http://wsmoak.net/2016/09/13/manipulate-dates-and-times-with-elixir.html). ¡Diviértete programando!