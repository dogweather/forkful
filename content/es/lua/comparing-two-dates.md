---
title:                "Comparando dos fechas"
html_title:           "Lua: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Comparar dos fechas en programación significa verificar si una fecha es mayor, menor o igual a otra. Los programadores necesitan hacer esto para ordenar y filtrar datos, o para determinar si se ha pasado una fecha límite. Es una tarea común en muchas aplicaciones.

## Cómo:

```Lua
-- Ejemplo 1: Comparando dos fechas utilizando operadores lógicos
if fecha1 > fecha2 then
  print("La fecha 1 es después de la fecha 2")
elseif fecha1 < fecha2 then
  print("La fecha 1 es antes de la fecha 2")
else
  print("Las dos fechas son iguales")
end

-- Ejemplo 2: Utilizando funciones para convertir fechas en segundos desde la época y luego compararlos
-- Esta técnica es útil cuando las fechas están en diferentes formatos
-- Fuente: https://www.lua.org/pil/22.1.html
function fechaEnSegundos(fecha)
  local dateTable = os.date("*t", fecha)
  return os.time(dateTable)
end

-- Comparando dos fechas 
if fechaEnSegundos(fecha1) > fechaEnSegundos(fecha2) then
  print("La fecha 1 es después de la fecha 2")
elseif fechaEnSegundos(fecha1) < fechaEnSegundos(fecha2) then
  print("La fecha 1 es antes de la fecha 2")
else
  print("Las dos fechas son iguales")
end

```

## Inmersión profunda:

En la programación, las fechas se almacenan y se manipulan como valores numéricos que representan segundos desde la "época". La época es un punto de referencia desde el cual se cuenta el tiempo. En Lua, la época es el 1 de enero de 1970 a las 00:00:00 UTC. A partir de allí, se pueden utilizar operadores lógicos como <, > y == para comparar fechas. Otra técnica es convertir las fechas en segundos y luego compararlos, como se muestra en el ejemplo 2.

Una alternativa a la comparación de fechas es el uso de librerías especiales de fecha y hora, como `luatz` o `date`. Estas librerías ofrecen funciones más avanzadas para operaciones con fechas y pueden ser más útiles en casos de uso complejos.

## Vea también:

[Documentación de Lua sobre fechas y hora](https://www.lua.org/manual/5.3/manual.html#6.9)

[`luatz`]: https://github.com/daurnimator/luatz

[`date`]: http://tichy.tv/php/datebook/