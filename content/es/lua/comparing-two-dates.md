---
title:                "Comparando dos fechas"
html_title:           "C#: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Comparar dos fechas en programación significa determinar si una fecha es mayor, menor o igual a otra. Los programadores realizan esta acción para ordenar eventos, filtrar información basada en el tiempo, entre otras tareas.

## Cómo se hace:

Lua no tiene una biblioteca estándar para manejar fechas y horas, pero puedes usar la biblioteca `os`. Aquí hay un simple ejemplo de cómo comparar dos fechas:

```Lua
d1 = os.time({year=2022, month=5, day=10})
d2 = os.time({year=2023, month=5, day=10})

if(d1 > d2) then
    print("La fecha 1 es mayor que la fecha 2")
elseif (d1 < d2) then
    print("La fecha 1 es menor que la fecha 2")
else
    print("Las dos fechas son iguales")
end
```

En este caso, deberías ver "La fecha 1 es menor que la fecha 2" como resultado.

## Inmersión Profunda  

Históricamente, Lua se diseñó para ser un lenguaje pequeño y ligero, por lo que la biblioteca `os` tiene una funcionalidad limitada para fechas y horas. Sin embargo, hay bibliotecas de terceros para manejar fechas de manera más flexible, como `luadate` y `Penlight`.

La función `os.time` utilizada en el ejemplo anterior convierte una tabla de fecha en segundos pasados desde una época, que es 1° de enero de 1970 por defecto. Esto nos da un valor numérico que podemos usar para comparar fácilmente dos fechas.

Antes de usar la función `os.time`, por favor, asegúrate de entender los límites de su uso. Para fechas antes de 1970 o después de 2038, `os.time` puede tener un comportamiento inconsistentes en diferentes sistemas.

## Vea También:

Para más información sobre comparación de fechas en Lua:

1. Documentación oficial de Lua `os.date` y `os.time`: https://www.lua.org/pil/22.1.html
2. Biblioteca 'luadate': https://github.com/Tieske/date
3. Biblioteca 'Penlight': http://stevedonovan.github.io/Penlight/api/index.html