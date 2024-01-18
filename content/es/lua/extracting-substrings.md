---
title:                "Extrayendo subcadenas"
html_title:           "Lua: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Extraer subcadenas es una técnica común en programación que consiste en obtener una parte específica de una cadena de texto. Los programadores lo hacen para acceder a información específica dentro de una cadena más grande o manipular y modificar cadenas.

## Cómo hacerlo:
```Lua
-- Ejemplo 1: Obtener una subcadena a partir de un índice
local cadena = "Hola mundo"
local subcadena = string.sub(cadena, 5) -- "mundo"

-- Ejemplo 2: Obtener una subcadena a partir de un índice y una longitud
local cadena = "Hola mundo"
local subcadena = string.sub(cadena, 2, 4) -- "ola"

-- Ejemplo 3: Reemplazar una subcadena en una cadena
local cadena = "Hola mundo"
local nueva_cadena = string.gsub(cadena, "mundo", "Lua") -- "Hola Lua"
```

## Profundizando:
La extracción de subcadenas ha existido desde los primeros lenguajes de programación, donde se utilizaba específicamente para acceder a caracteres individuales en una cadena. En Lua, además de la función `string.sub()` que hemos visto en los ejemplos, también hay otras opciones como `string.match()` y `string.gsub()` que se utilizan para buscar y manipular subcadenas de manera más específica.

## Ver también:
- [Documentación de Lua - Funciones de cadenas](https://www.lua.org/manual/5.4/manual.html#6.4)
- [Tutorial de extracción de subcadenas en Lua](https://riptutorial.com/es/lua/example/15164/extraer-partes-de-cadenas)