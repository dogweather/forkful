---
title:                "Elixir: Calculando una fecha en el futuro o pasado"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has necesitado calcular una fecha en el futuro o en el pasado en tu código de Elixir? Quizás estabas creando una función para programar una tarea en una fecha específica o necesitabas mostrar una fecha en un formato más legible para los usuarios. Sea cual sea el caso, la capacidad de calcular fechas es una habilidad importante en la programación y en este artículo aprenderás cómo hacerlo en Elixir.

## Cómo hacerlo

En Elixir, existen dos módulos que nos permiten trabajar con fechas: `Date` y `DateTime`. La diferencia entre ellos es que `DateTime` incluye información adicional como la hora y la zona horaria, mientras que `Date` solo se enfoca en la fecha en sí.

Para calcular una fecha en el futuro o en el pasado, primero debemos obtener la fecha actual utilizando la función `~U[YYYY-MM-DD]`, donde reemplazamos los valores entre corchetes con el año, mes y día actual. Luego, podemos utilizar la función `add/2` para sumar o restar una cantidad específica de días o meses a la fecha actual.

Veamos un ejemplo de cómo calcular una fecha en el futuro y en el pasado utilizando el módulo `Date`:

```Elixir
~U[2021-11-10] |> Date.add(10) # Suma 10 días a la fecha actual
# Salida: ~U[2021-11-20]

~U[2021-11-10] |> Date.add(-3, :months) # Resta 3 meses a la fecha actual
# Salida: ~U[2021-08-10]
```

También podemos especificar una fecha específica en lugar de la fecha actual al utilizar la función `~D[YYYY-MM-DD]` en lugar de `~U[YYYY-MM-DD]`. Ahora, veamos cómo calcular una fecha en el futuro y en el pasado utilizando el módulo `DateTime`:

```Elixir
~N[2021-11-10 15:00:00] |> DateTime.add(2, :days) # Suma 2 días y 15 horas a la fecha actual
# Salida: ~N[2021-11-12 06:00:00]

~N[2021-11-10 15:00:00] |> DateTime.add(-1, :week) # Resta 1 semana a la fecha actual
# Salida: ~N[2021-11-03 15:00:00]
```

## Profundizando

Ahora que ya sabemos cómo calcular fechas en Elixir, es importante tener en cuenta algunos detalles adicionales. Primero, podemos utilizar cualquier tipo de operaciones matemáticas para sumar o restar valores de fecha, no solo días o meses. Incluso podemos utilizar fracciones de días para obtener resultados más precisos.

También podemos utilizar la función `add/3` en lugar de `add/2` para especificar un tercer argumento, que puede ser `:minutes`, `:hours`, `:days`, `:weeks` o `:months`, y así obtener un resultado más específico.

Además, es importante mencionar que Elixir maneja las fechas en formato ISO 8601, lo que significa que utiliza el estándar internacional para representación y almacenamiento de fechas y horas.

## Ver también

- [Documentación oficial de Elixir sobre el módulo Date](https://hexdocs.pm/elixir/Date.html)
- [Documentación oficial de Elixir sobre el módulo DateTime](https://hexdocs.pm/elixir/DateTime.html)
- [ISO 8601](https://es.wikipedia.org/wiki/ISO_8601)