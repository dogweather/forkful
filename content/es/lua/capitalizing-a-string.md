---
title:                "Capitalizando una cadena de texto"
date:                  2024-01-19
html_title:           "Arduino: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Capitalizar una cadena significa convertir la primera letra de cada palabra a mayúscula. Los programadores lo hacen para formatear títulos o nombres propios, asegurando uniformidad y legibilidad en el texto.

## Cómo hacer:
Aquí tienes un ejemplo de cómo capitalizar una cadena en Lua:

```Lua
function capitalizar(cadena)
    return (cadena:gsub("%f[%a](%a)", function(letra) return letra:upper() end))
end

-- Uso de la función
local frase = "lua es divertido"
print(capitalizar(frase))  -- Output: Lua Es Divertido
```

## Inmersión Profunda
En el pasado convierte rápidamente las cadenas a mayúsculas con funciones básicas. Hoy Lua no tiene una función incorporada para capitalizar, así que creamos una usando patrones de coincidencia (`gsub`) y manipulando cada palabra.

Alternativas incluyen usar librerías externas o escribir funciones adicionales para manejar casos especiales, como abreviaturas o acrónimos.

La función `gsub` es poderosa; permite modificar texto con gran precisión. En nuestro caso, `%f[%a]` es un patrón que encuentra la frontera entre un carácter no alfabético y uno alfabético, y `(%a)` coincide con la primera letra alfabética encontrada.

## Ver También
- [Documentación oficial de Lua](https://www.lua.org/manual/5.4/)
- [Tutorial de los patrones de Lua](https://www.lua.org/pil/20.2.html)
- [Foros de la comunidad Lua](http://www.lua.org/community.html)
