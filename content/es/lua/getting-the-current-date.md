---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:15:53.643126-07:00
html_title:           "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Obtener la fecha actual en un programa es simplemente saber qué día es hoy en formato año, mes y día. Los programadores lo hacen para registrar eventos, comparar fechas o simplemente mostrar información relevante basada en la fecha actual.

## How to:
Para obtener la fecha actual en Lua, puedes usar la función `os.date()`. Aquí te muestro cómo:

```Lua
local fecha_actual = os.date("%Y-%m-%d") -- Formato ISO de fecha: AAAA-MM-DD
print("La fecha de hoy es: " .. fecha_actual)
```
Ejemplo de salida:
```
La fecha de hoy es: 2023-04-06
```
Si quieres más información, como la hora, minutos y segundos, puedes hacer lo siguiente:
```Lua
local fecha_hora_actual = os.date("%Y-%m-%d %H:%M:%S")
print("Fecha y hora actuales: " .. fecha_hora_actual)
```
Ejemplo de salida:
```
Fecha y hora actuales: 2023-04-06 15:45:30
```

## Deep Dive:
Los métodos de fecha y hora en Lua están basados en la biblioteca estándar de C. Lua proporciona varias funciones relacionadas con el tiempo en su biblioteca de sistema `os`, y `os.date()` es una de ellas.

Historicamente, Lua no siempre tuvo esta funcionalidad incorporada, y en versiones muy antiguas, los desarrolladores tenían que depender de bibliotecas de terceros o implementaciones propias.

Como alternativa a `os.date()`, podrías usar `os.time()` para obtener la fecha y hora actuales en forma de sello de tiempo (timestamp), y luego convertirlo en un formato legible. 

La implementación interna de `os.date()` utiliza la función `strftime()` de C, que permite una gran flexibilidad en los formatos de fecha y hora. Además, la función `os.date("*t")` retorna una tabla con separación detallada de elementos de la fecha y hora actual, permitiendo acceso individual a año, mes, día, etc.

## See Also:
- [Referencia de Lua `os.date()`](https://www.lua.org/manual/5.4/manual.html#pdf-os.date)
- [Tutorial de los tipos de datos en Lua](https://www.tutorialspoint.com/lua/lua_data_types.htm)
- [Documentación de Lua sobre la biblioteca `os`](https://www.lua.org/pil/22.1.html)