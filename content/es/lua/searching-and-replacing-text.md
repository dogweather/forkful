---
title:                "Buscando y reemplazando texto"
html_title:           "Lua: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué se hace?
Buscar y reemplazar texto es una herramienta común en la programación para encontrar y cambiar repeticiones de texto en un código. Los programadores lo hacen para ahorrar tiempo y evitar errores en su código.

## ¡Cómo hacerlo!
Para buscar y reemplazar texto en Lua, podemos usar la función incorporada `string.gsub`. Esta función toma dos argumentos: una cadena de texto para buscar y una cadena de texto para reemplazarla. Por ejemplo:

```
Lua
texto = "Hola, soy Lua."
nuevo_texto = string.gsub(texto, "Lua", "un lenguaje de programación")
print(nuevo_texto)
```
```
Salida:
Hola, soy un lenguaje de programación.
```

## Inmersión Profunda
Originalmente, la búsqueda y reemplazo de texto se hizo a mano antes de que existieran herramientas automatizadas. En Lua, también podemos usar expresiones regulares para buscar patrones específicos de texto. Otras alternativas incluyen el uso de editores de texto avanzados o scripts personalizados.

## Ver También
- [Documentación oficial de Lua en la función string.gsub] (https://www.lua.org/manual/5.4/manual.html#pdf-string.gsub)
- [Expresiones regulares en Lua] (https://www.tutorialspoint.com/lua/lua_pattern_matching.htm)