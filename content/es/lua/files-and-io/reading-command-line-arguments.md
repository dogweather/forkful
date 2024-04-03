---
date: 2024-01-20 17:56:32.611078-07:00
description: "C\xF3mo hacerlo: Para leer argumentos de la l\xEDnea de comandos en\
  \ Lua, se utiliza la tabla global `arg`. Aqu\xED un ejemplo b\xE1sico."
lastmod: '2024-03-13T22:44:59.217818-06:00'
model: gpt-4-1106-preview
summary: "Para leer argumentos de la l\xEDnea de comandos en Lua, se utiliza la tabla\
  \ global `arg`."
title: "Lectura de argumentos de l\xEDnea de comandos"
weight: 23
---

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
