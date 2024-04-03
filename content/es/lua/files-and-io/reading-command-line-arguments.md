---
date: 2024-01-20 17:56:32.611078-07:00
description: "Leer argumentos de la l\xEDnea de comandos permite a tu programa Lua\
  \ recibir informaci\xF3n externa al momento de ser ejecutado, brind\xE1ndote flexibilidad.\
  \ Los\u2026"
lastmod: '2024-03-13T22:44:59.217818-06:00'
model: gpt-4-1106-preview
summary: "Leer argumentos de la l\xEDnea de comandos permite a tu programa Lua recibir\
  \ informaci\xF3n externa al momento de ser ejecutado, brind\xE1ndote flexibilidad."
title: "Lectura de argumentos de l\xEDnea de comandos"
weight: 23
---

## ¿Qué es y Por Qué?
Leer argumentos de la línea de comandos permite a tu programa Lua recibir información externa al momento de ser ejecutado, brindándote flexibilidad. Los programadores lo usamos para personalizar la ejecución sin cambiar el código y para interactuar con el sistema operativo o scripts.

## Cómo hacerlo:
Para leer argumentos de la línea de comandos en Lua, se utiliza la tabla global `arg`. Aquí un ejemplo básico:

```Lua
-- script.lua
for i = 1, #arg do
  print(i, arg[i])
end
```

Si ejecutas este script en la terminal así:

```
lua script.lua hola mundo
```

Obtendrás como salida:

```
1   hola
2   mundo
```

## Inmersión Profunda:
Históricamente, los argumentos de línea de comandos son una de las primeras formas de interactuar con programas. En Lua, la tabla `arg` almacena estos argumentos y también los campos `arg[-1]`, `arg[0]` que representan el comando y el nombre del script, respectivamente.

Lua no tiene una biblioteca estándar para parsear argumentos más complejos (como banderas `-a value`), pero hay múltiples bibliotecas de terceros que puedes integrar.

Cabe destacar que los elementos en `arg` son siempre strings. Si necesitas otros tipos, tendrás que convertirlos manualmente. Por ejemplo:

```Lua
-- Suponiendo que necesitas un número y un booleano
local num = tonumber(arg[1])
local flag = arg[2] == 'true'
```

Si tu programa va a ser usado con frecuencia y necesita opciones más sofisticadas, considerar una librería para gestionar argumentos podría ser una inversión que ahorra tiempo.

## Ver También:
- [Programming in Lua (Fourth edition)](https://www.lua.org/pil/contents.html), para entender más sobre los fundamentos de Lua.
- [LuaRocks](https://luarocks.org/), donde puedes buscar paquetes como `argparse` si necesitas más funcionalidad para manejar argumentos.
- [La documentación oficial de Lua](https://www.lua.org/manual/5.4/) para los más cuadriculados en obtener la información de primera mano.
