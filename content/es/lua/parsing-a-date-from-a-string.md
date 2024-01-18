---
title:                "Analizando una fecha de una cadena"
html_title:           "Lua: Analizando una fecha de una cadena"
simple_title:         "Analizando una fecha de una cadena"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Parsear una fecha a partir de un string es convertir un texto que contiene una fecha en un formato específico en una estructura de datos que la represente de manera que sea manipulable por la computadora. Los programadores realizan esto para poder trabajar con fechas en sus programas de manera más eficiente y precisa.

## Cómo:
```
-- Ejemplo de cómo parsear una fecha en Lua
local fecha = "13 de junio de 2021"
local dia, mes, año = string.match(fecha, "(%d+) de (%a+) de (%d+)")

-- Imprime el resultado
print("Día: " .. dia)
print("Mes: " .. mes)
print("Año: " .. año)

-- Output:
-- Día: 13
-- Mes: junio
-- Año: 2021
```

## Profundizando:
Parsear fechas a partir de strings es una técnica comúnmente utilizada en la programación. Antes de la popularización de los lenguajes de programación de alto nivel, esta tarea requería manuales detallados de algoritmos específicos. En la actualidad, existen librerías y módulos que facilitan esta tarea en diferentes lenguajes, incluyendo Lua. En lugar de parsear manualmente, también se pueden utilizar librerías externas para manejar fechas en formatos específicos.

## Ver también:
- [Función string.match en la documentación de Lua](https://www.lua.org/manual/5.4/manual.html#pdf-string.match)
- [Tutorial de parseo de fechas en Lua](https://www.tutorialspoint.com/lua/lua_strings.htm)
- [Librería externa para manejar fechas en Lua](https://luarocks.org/modules/tieske/date)