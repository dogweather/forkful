---
title:                "Eliminando caracteres que coinciden con un patrón"
date:                  2024-01-20T17:42:36.497823-07:00
model:                 gpt-4-1106-preview
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Eliminar caracteres que coinciden con un patrón es básicamente buscar parte de un texto que se ajusta a ciertas reglas y luego quitarlo. Lo hacemos para limpiar datos, extraer información relevante o manipular cadenas de texto de acuerdo con nuestras necesidades.

## Cómo hacerlo:

```Lua
local texto = "Hola mundo! 1234"
local patron = "%D" -- Patrón para eliminar todos los no dígitos

-- Usamos gsub para reemplazar el patrón con una cadena vacía
local resultado = texto:gsub(patron, "")

print(resultado) -- Output: 1234
```

En este ejemplo, `%D` busca todos los caracteres que no son dígitos. `gsub` los reemplaza por nada (los elimina).

```Lua
local frase = "Lua es genial! 2023, sí."
local patron = "[%d%p]" -- Patrón para eliminar todos los dígitos y puntuaciones

local limpio = frase:gsub(patron, "")

print(limpio) -- Output: Lua es genial sí
```

Aquí, `[%d%p]` combina dígitos `%d` y puntuaciones `%p`, y los elimina de la cadena de texto.

## Deep Dive

Lua utiliza patrones que se parecen a las expresiones regulares, pero son más simples. Desde su introducción, han sido una herramienta poderosa para el manejo de cadenas de texto. `gsub` es la función estrella para reemplazar texto: toma un patrón para buscar y una cadena de reemplazo (que puede ser vacía para eliminar).

Existen alternativas a usar patrones en Lua, como hacer ciclos a través de cada carácter o usar funciones de bibliotecas externas. Sin embargo, la simplicidad y eficiencia de `gsub` suelen ser insuperables.

Los patrones de Lua tienen sus propios matices, como los conjuntos de caracteres `%a` (letras), `%s` (espacios), etc., y su capacidad para modificar la cantidad de coincidencias con `+`, `*`, y `-`. Entender bien estos elementos es clave para manipular texto de manera efectiva.

## See Also

- [Referencia oficial de Lua (5.4)](https://www.lua.org/manual/5.4/)
- [Guía de patrones en Lua](https://www.lua.org/pil/20.2.html)
