---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Lua: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Calcular una fecha en el futuro o pasado es una función fundamental en la programación, ya que permite a los desarrolladores manipular fechas y crear aplicaciones que requieran cálculos relacionados con el tiempo. Es especialmente útil para aplicaciones como calendarios, recordatorios y programación de eventos.

## ¿Cómo hacerlo?

Para calcular una fecha en el futuro o pasado en Lua, podemos utilizar la función `os.time()` y `os.date()`. Ambas funciones requieren de un parámetro que especifique un formato de fecha y horario, y devolverá la fecha y hora actual en segundos o en una tabla, respectivamente. 

```Lua
-- Ejemplo de calcular una fecha en el futuro utilizando os.time:
local segundos_en_un_dia = 24 * 60 * 60 -- 24 horas x 60 minutos x 60 segundos
local fecha_en_un_dia = os.time() + segundos_en_un_dia
print(os.date("%d/%m/%Y", fecha_en_un_dia)) -- imprime la fecha exacta en un día

-- Ejemplo de calcular una fecha en el pasado utilizando os.date:
local fecha_en_5_dias = os.date("*t", os.time()) -- convierte los segundos en una tabla de fecha y hora
fecha_en_5_dias.day = fecha_en_5_dias.day - 5 -- resta 5 días a la fecha actual
print(os.date("%d/%m/%Y", os.time(fecha_en_5_dias))) -- imprime la fecha exacta hace 5 días
```

El código anterior es solo un ejemplo básico de cómo calcular una fecha en el futuro o pasado utilizando Lua. Se pueden realizar cálculos más complejos según las necesidades de cada aplicación.

## Detalles más profundos

Lua cuenta con numerosas funciones y librerías para trabajar con fechas y horarios, como `os.date()`, `os.time()`, `os.clock()`, `os.difftime()`, entre otras. Estas funciones se basan en el sistema Unix, donde las fechas se calculan en segundos desde el 1 de enero de 1970 a la 00:00 GMT. 

Además, existen librerías externas como "LuaDate" que ofrecen más opciones avanzadas para el manejo de fechas, incluyendo cálculos de zonas horarias y fechas astronómicas.

Otra opción es usar la librería de tiempo de LuaJIT, que ofrece una precisión hasta el nivel de nanosegundos en cálculos de tiempo. Sin embargo, esta librería solo está disponible para la versión de LuaJIT de Lua.

## Ver también

- Documentación de Lua oficial (https://www.lua.org/docs.html)
- Librería LuaDate (https://keplerproject.github.io/lua_date/)
- Librería de tiempo de LuaJIT (http://luajit.org/ext_ffi_semantics.html#new-ctypes-ffi-timehandling)