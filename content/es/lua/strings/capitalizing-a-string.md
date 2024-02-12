---
title:                "Capitalizando una cadena de texto"
aliases:
- /es/lua/capitalizing-a-string.md
date:                  2024-02-03T19:05:49.390749-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizando una cadena de texto"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Capitalizar una cadena implica modificar el primer carácter de cada palabra en una oración para que sea mayúscula, mientras se asegura de que el resto sean minúsculas. Esta técnica se utiliza comúnmente para formatear texto para que tenga una salida más profesional o legible, como preparar títulos o entradas de usuario para su visualización.

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
