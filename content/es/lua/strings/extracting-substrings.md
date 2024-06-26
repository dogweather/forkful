---
date: 2024-01-20 17:46:00.771951-07:00
description: "C\xF3mo: Lua tiene varias funciones para manejar cadenas, pero `string.sub`\
  \ es la m\xE1s directa para extraer subcadenas. Desde sus primeras versiones, Lua\
  \ ha\u2026"
lastmod: '2024-04-05T21:54:00.537118-06:00'
model: gpt-4-1106-preview
summary: "Lua tiene varias funciones para manejar cadenas, pero `string.sub` es la\
  \ m\xE1s directa para extraer subcadenas."
title: "Extracci\xF3n de subcadenas"
weight: 6
---

## Cómo:
```Lua
-- Ejemplo 1: Extraer una parte de una cadena
local texto = "Lua es divertido"
local subcadena = texto:sub(5, 7)
print(subcadena)  -- Salida: "es"

-- Ejemplo 2: Extraer hasta el final de una cadena
local subcadena_final = texto:sub(9)
print(subcadena_final)  -- Salida: "divertido"

-- Ejemplo 3: Uso de índices negativos
local subcadena_negativos = texto:sub(-9, -1)
print(subcadena_negativos)  -- Salida: "divertido"
```

## Profundizando
Lua tiene varias funciones para manejar cadenas, pero `string.sub` es la más directa para extraer subcadenas. Desde sus primeras versiones, Lua ha facilitado la manipulación de textos, y los índices negativos heredan de C la idea de contar desde el final. Otras funciones como `string.match` y las expresiones regulares en lenguajes como Perl ofrecen enfoques alternativos que pueden ser más flexibles pero también más complejos. A nivel de implementación, al extraer subcadenas, Lua intenta ser eficiente, evitando la creación de múltiples copias de la cadena original siempre que sea posible.

## Ver También
- Referencia oficial de Lua 5.4 (última versión): https://www.lua.org/manual/5.4/
- Tutorial sobre cadenas en Lua: https://www.tutorialspoint.com/lua/lua_strings.htm
- Documentación sobre patrones de Lua (útil para `string.match`): https://www.lua.org/pil/20.2.html
