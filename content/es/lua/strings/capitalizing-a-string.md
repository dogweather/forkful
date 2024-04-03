---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:49.390749-07:00
description: "C\xF3mo hacerlo: Lua no tiene una funci\xF3n integrada para capitalizar\
  \ cadenas, pero puedes lograr f\xE1cilmente esta tarea utilizando funciones b\xE1\
  sicas de\u2026"
lastmod: '2024-03-13T22:44:59.185733-06:00'
model: gpt-4-0125-preview
summary: "Lua no tiene una funci\xF3n integrada para capitalizar cadenas, pero puedes\
  \ lograr f\xE1cilmente esta tarea utilizando funciones b\xE1sicas de manipulaci\xF3\
  n de cadenas."
title: Capitalizando una cadena de texto
weight: 2
---

## Cómo hacerlo:
Lua no tiene una función integrada para capitalizar cadenas, pero puedes lograr fácilmente esta tarea utilizando funciones básicas de manipulación de cadenas. Aquí hay una función simple para capitalizar la primera letra de una sola palabra:

```lua
function capitalize(word)
    return word:sub(1,1):upper() .. word:sub(2):lower()
end

print(capitalize("hello"))  -- Salida: Hello
```

Para capitalizar cada palabra en una oración, puedes dividir la oración en palabras, capitalizar cada una y luego volver a unirlas:

```lua
function capitalizeSentence(sentence)
    local words = {}
    for word in sentence:gmatch("%S+") do
        table.insert(words, capitalize(word))
    end
    return table.concat(words, " ")
end

print(capitalizeSentence("hello world from lua"))  -- Salida: Hello World From Lua
```

Si estás trabajando en un proyecto donde el rendimiento es clave y te encuentras necesitando capacidades de manipulación de cadenas más avanzadas, considera usar una biblioteca de terceros como `Penlight`. Penlight mejora Lua con funciones de manejo de cadenas más versátiles, entre otras utilidades:

```lua
-- Asumiendo que Penlight está instalado:
local pl = require("pl.stringx")
local text = "hello lua users"
text = pl.capitalized(text)
print(text)  -- Salida: Hello lua users

-- Nota: La función capitalized de Penlight solo capitaliza la primera palabra.
-- Para capitalizar cada palabra, todavía deberías implementar una solución personalizada o explorar otras bibliotecas.
```
