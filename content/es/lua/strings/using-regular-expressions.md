---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:27.349636-07:00
description: "Las expresiones regulares en la programaci\xF3n permiten la b\xFAsqueda\
  \ y manipulaci\xF3n de cadenas basadas en patrones espec\xEDficos. Los programadores\
  \ las\u2026"
lastmod: '2024-03-13T22:44:59.192112-06:00'
model: gpt-4-0125-preview
summary: "Las expresiones regulares en la programaci\xF3n permiten la b\xFAsqueda\
  \ y manipulaci\xF3n de cadenas basadas en patrones espec\xEDficos."
title: Usando expresiones regulares
weight: 11
---

## Cómo hacerlo:
Lua no admite expresiones regulares de manera nativa de la misma forma que lenguajes como Perl o Python. En su lugar, ofrece capacidades de coincidencia de patrones que cubren muchos casos de uso comunes de las expresiones regulares. Sin embargo, para obtener soporte completo de expresiones regulares, se puede usar una biblioteca de terceros como `lrexlib`.

### Coincidencia de Patrones Básica en Lua:
Lua proporciona un sistema de coincidencia de patrones poderoso que puedes utilizar para sustituciones y búsquedas simples:

```lua
-- Búsqueda simple
local str = "Hola, Mundo!"
if string.find(str, "Mundo") then
  print("¡Coincidencia encontrada!")
end
-- Salida: ¡Coincidencia encontrada!

-- Sustitución simple
local s = string.gsub("¡Lua es genial!", "genial", "increíble")
print(s)
-- Salida: ¡Lua es increíble!
```

### Capturando Subcadenas:
Puedes capturar partes de la cadena que coincidan con patrones:

```lua
local fecha = "Hoy es 17/05/2023."
local d, m, a = string.match(fecha, "(%d+)/(%d+)/(%d+)")
print("Día:", d, "Mes:", m, "Año:", a)
-- Salida: Día: 17 Mes: 05 Año: 2023
```

### Usando `lrexlib` para Expresiones Regulares:
Para usar expresiones regulares reales, puedes instalar y usar `lrexlib`. Suponiendo que lo tengas instalado (`luarocks install lrexlib-pcre`), puedes realizar coincidencias de patrones más complejas:

```lua
local rex = require 'rex_pcre'

local texto = "La lluvia en España permanece principalmente en la llanura."
local regex = "\\bS\\w+"
local cuenta, err = rex.gsub(texto, regex, function(w)
  return w:upper()
end)
if err then
  print("Error:", err)
else
  print("Texto modificado:", texto)
  print("Sustituciones realizadas:", cuenta)
end
-- Ejemplo de salida: Texto modificado: La lluvia en ESPAÑA permanece PRINCIPALMENTE en la llanura.
-- Sustituciones realizadas: 3
```

Los ejemplos anteriores ilustran el uso básico dentro del propio sistema de coincidencia de patrones de Lua y cómo aprovechar el poder de las expresiones regulares a través de `lrexlib`. Ya sea que estés realizando manipulaciones simples de cadenas o requieras de toda la versatilidad de las expresiones regulares, Lua, junto con bibliotecas poderosas, puede satisfacer tus necesidades.
