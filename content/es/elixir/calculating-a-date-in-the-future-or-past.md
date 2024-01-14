---
title:    "Elixir: Calculando una fecha en el futuro o pasado"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Por qué

Las fechas son una parte importante de cualquier aplicación. Ya sea que estemos registrando eventos en un calendario o calculando el tiempo transcurrido desde una fecha específica, trabajar con fechas es una tarea común en la programación. En este artículo, exploraremos cómo podemos utilizar Elixir para calcular una fecha en el futuro o en el pasado.

# Cómo hacerlo

Primero, necesitaremos importar el módulo Date de Elixir para acceder a sus funciones. Luego, podemos utilizar la función `add` para calcular una fecha en el futuro o `sub` para calcular una fecha en el pasado. Ambas funciones toman tres argumentos: la fecha de partida, la cantidad de tiempo a agregar o restar y la unidad de tiempo (días, meses o años).

```Elixir
# Importar el módulo Date
import Date

# Calcular fecha en el futuro
future_date = add(Date.utc_today(), 5, :days)
# 2022-11-23

# Calcular fecha en el pasado
past_date = sub(Date.utc_today(), 2, :years)
# 2019-11-23
```

Como se puede ver en los ejemplos anteriores, podemos especificar la unidad de tiempo utilizando los átomos `:days`, `:months` o `:years`. También podemos utilizar números negativos para restar tiempo en lugar de agregarlo.

# Profundizando

Detrás de escena, la función `add` y `sub` utilizan la función `add_unit` del módulo `Calendar` para realizar los cálculos. Esta función toma como argumentos una fecha y una estructura llamada `caldendar_interval` para especificar la cantidad de tiempo y la unidad de tiempo.

En nuestro ejemplo, `Date.utc_today()` se convierte en una estructura de `calendar_interval` con `:day` como unidad de tiempo. Luego, la función `add_unit` es llamada para realizar el cálculo y proporciona el resultado.

# Ver También

- Documentación oficial de Elixir sobre fechas: https://hexdocs.pm/elixir/Date.html
- Cómo utilizar el módulo `Calendar`: https://elixir-lang.org/getting-started/calendar.html