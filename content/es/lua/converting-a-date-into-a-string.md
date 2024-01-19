---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Convertir una fecha a un string en programación es trasformar un objeto de fecha, que incluye día, mes y año en texto. Los programadores hacen esto para una fácil lectura, manipulación y almacenamiento.

## Cómo hacerlo:

Convertir una fecha a string en Lua es simple. Usamos la función `os.date()` con un objeto `os.time()` como entrada.

```Lua
local tiempo = os.time({year=2022, month=3, day=14})
local fecha = os.date('%Y-%m-%d', tiempo)
print(fecha) -- 2022-03-14
```

La función `os.date()` utiliza códigos de formato. En este caso, `%Y` para el año, `%m` para el mes, y `%d` para el día.

## Análisis en profundidad:

La funcionalidad de convertir fechas a strings ha estado en Lua desde su primera versión, y aún es esencial para muchos usos de desarrollo.

Alternativamente, puedes formatear la fecha a tu gusto usando diferentes códigos de formato.

```Lua
local tiempo = os.time({year=2022, month=3, day=14, hour=13, min=52, sec=30})
local fecha = os.date('%B %d, %Y %I:%M:%S %p', tiempo)
print(fecha) -- March 14, 2022 01:52:30 PM
```

Asegúrate de tener en cuenta las limitaciones de la función `os.time()`. Solo admite fechas desde 1970 y puede no funcionar con fechas antes de este año, ya que está basado en el sistema de tiempo Unix.

## Ver también:

- Documentación de Lua `os.date()`: https://www.lua.org/manual/5.4/manual.html#6.9
- Documentación de Lua `os.time()`: https://www.lua.org/manual/5.4/manual.html#6.8
- Listado de códigos de formato: https://www.lua.org/pil/22.1.html
- Artículo sobre tiempo Unix: https://en.wikipedia.org/wiki/Unix_time