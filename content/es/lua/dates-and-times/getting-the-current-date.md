---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:04.348085-07:00
description: "Obtener la fecha actual en la programaci\xF3n es una tarea crucial para\
  \ una multitud de aplicaciones, incluyendo el registro de actividades, la estampaci\xF3\
  n\u2026"
lastmod: '2024-03-13T22:44:59.212371-06:00'
model: gpt-4-0125-preview
summary: "Obtener la fecha actual en la programaci\xF3n es una tarea crucial para\
  \ una multitud de aplicaciones, incluyendo el registro de actividades, la estampaci\xF3\
  n de tiempo en eventos o la programaci\xF3n de tareas."
title: Obteniendo la fecha actual
weight: 29
---

## Cómo hacerlo:
Lua proporciona la función `os.date` para obtener la fecha y hora actuales. La función se puede usar sin argumentos para obtener una cadena formateada o con especificadores de formato para personalizar la salida. Así es cómo usarla:

```lua
-- Obtener la fecha y hora actuales como una cadena formateada
print(os.date())  -- p.ej., Jue Mar  3 14:02:03 2022

-- Personalizando el formato de salida
-- %Y para año, %m para mes, %d para día, %H para hora, %M para minutos
print(os.date("%Y-%m-%d %H:%M"))  -- p.ej., 2022-03-03 14:02
```

Para manipulaciones más sofisticadas de fecha y hora, Lua no tiene bibliotecas integradas tan ricas como algunos otros lenguajes de programación. Sin embargo, puedes usar bibliotecas de terceros como `lua-date` (https://github.com/Tieske/date). Esta biblioteca ofrece funcionalidades más completas para manipular fechas y horas. Así es cómo podrías usarla:

Primero, asegúrate de haber instalado la biblioteca `lua-date`. Típicamente, puedes instalarla usando LuaRocks con el siguiente comando:

```bash
luarocks install lua-date
```

Luego, puedes usarla en tu script de Lua de la siguiente manera:

```lua
local date = require("date")

-- Creando un objeto de fecha para la fecha y hora actuales
local now = date()

print(now:fmt("%Y-%m-%d %H:%M:%S"))  -- p.ej., 2022-03-03 14:02:03
```

Este ejemplo demuestra la creación de un objeto `date` representando el momento actual, al cual luego puedes dar formato de manera similar a la función `os.date` pero con flexibilidad y opciones adicionales proporcionadas por la biblioteca `lua-date`.
