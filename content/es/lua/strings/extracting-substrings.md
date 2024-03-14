---
date: 2024-01-20 17:46:00.771951-07:00
description: "Extraer subcadenas significa sacar partes espec\xEDficas de un texto.\
  \ Los programadores lo hacen para procesar y manejar datos de forma precisa."
lastmod: '2024-03-13T22:44:59.191211-06:00'
model: gpt-4-1106-preview
summary: "Extraer subcadenas significa sacar partes espec\xEDficas de un texto. Los\
  \ programadores lo hacen para procesar y manejar datos de forma precisa."
title: "Extracci\xF3n de subcadenas"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Extraer subcadenas significa sacar partes específicas de un texto. Los programadores lo hacen para procesar y manejar datos de forma precisa.

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
