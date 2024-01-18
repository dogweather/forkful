---
title:                "Capitalizando una cadena"
html_title:           "Lua: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Capitalizar una cadena de texto en Lua se refiere a convertir la primera letra de cada palabra en mayúscula. Los programadores suelen hacerlo para mejorar la legibilidad del código y seguir convenciones de escritura establecidas en el idioma en el que están programando.

## Cómo hacerlo:

Para capitalizar una cadena en Lua, puedes utilizar la función `string.upper()` que convierte una cadena completa a mayúsculas. Sin embargo, esto no capitaliza la primera letra de cada palabra, por lo que debes combinarla con la función `string.sub()` para obtener la primera letra y capitalizarla manualmente.

```
-- Ejemplo 1: Capitalizar una cadena con la primera letra en mayúscula
local cadena = "hola mundo"
cadena = string.upper(string.sub(cadena, 1, 1)) .. string.sub(cadena, 2)
print(cadena) -- Output: Hola mundo

-- Ejemplo 2: Capitalizar cada palabra de una cadena
local cadena = "mi lenguaje favorito es lua"
local nueva_cadena = ""
for palabra in cadena:gmatch("%S+") do -- Obtener cada palabra de la cadena
  palabra = string.upper(string.sub(palabra, 1, 1)) .. string.sub(palabra, 2)
  nueva_cadena = nueva_cadena .. palabra .. " "
end
print(nueva_cadena) -- Output: Mi Lenguaje Favorito Es Lua
```

## Profundizando:

En el pasado, el uso de mayúsculas para capitalizar palabras se remontaba a los escribas romanos en el siglo VII, quienes utilizaban letras mayúsculas para resaltar los nombres propios en sus escrituras. En la actualidad, la capitalización se utiliza principalmente para mejorar la legibilidad y seguir convenciones de escritura.

Otra forma de capitalizar una cadena en Lua es utilizando expresiones regulares con la función `string.gsub()` para reemplazar la primera letra de cada palabra por su versión mayúscula. Sin embargo, esto puede ser más complejo y se recomienda utilizar la función `string.upper()` en su lugar.

## Ver también:

- [string.upper()](https://www.lua.org/pil/20.1.html)
- [string.sub()](https://www.lua.org/pil/20.1.html)
- [string.gsub()](https://www.lua.org/pil/20.1.html)