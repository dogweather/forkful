---
title:                "Calcular una fecha en el futuro o pasado"
html_title:           "Lua: Calcular una fecha en el futuro o pasado"
simple_title:         "Calcular una fecha en el futuro o pasado"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Calcular una fecha en el futuro o pasado implica determinar una fecha específica a partir de hoy, sumando o restando un número específico de días, meses o años. Esto es crucial en la programación para operaciones tales como disparar recordatorios, calcular la edad, determinar fechas de vencimiento, entre otros.

## ¿Cómo hacerlo?

En Lua, puedes utilizar la biblioteca 'os' para calcular fechas en el futuro o en el pasado. Por ejemplo:

```Lua
os.date('%x',os.time()+60*60*24*7) -- Devuelve la fecha una semana desde hoy.
```

De igual manera, si deseas restar días, puedes hacerlo así:

```Lua
os.date('%x',os.time()-60*60*24*7) -- Devuelve la fecha una semana antes de hoy.
```

## Inmersión Profunda

Para entender cómo funciona esto, es útil tener un poco de contexto. El cálculo de fechas en Lua se basa en las capacidades innatas del sistema operativo, por lo que la función `os.time` devuelve una estampilla de tiempo Unix, que es el número de segundos transcurridos desde el 1 de enero de 1970.

Alternativamente, Lua ofrece la función `os.date`, que te permite obtener una representación de la fecha y hora en un formato más legible.

En cuanto a detalles de implementación, Lua cuenta las fechas y los horarios en segundos, y eso es lo que estamos ajustando al sumar o restar de `os.time`. Por llevar todo a una sola unidad (segundos), el cálculo se simplifica.

## Ver También

Para profundizar aún más en el cálculo de fechas con Lua, te recomiendo los siguientes recursos:

- Manual oficial de Lua: [os.time](https://www.lua.org/manual/5.4/manual.html#6.9)
- Documento sobre fechas y horas en Lua: [lua-users.org](http://lua-users.org/wiki/DateAndTime)