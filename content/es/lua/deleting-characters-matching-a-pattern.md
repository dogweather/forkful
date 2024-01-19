---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Eliminar caracteres que coincidan con un patrón es una forma de limpiar y manipular cadenas de texto en programación. Los programadores lo hacen para buscar, reemplazar o quitar caracteres específicos en un texto.

## Cómo hacerlo:

Usar la función `gsub()` en Lua para este objetivo es bastante simple. Aquí tienes un ejemplo:

```Lua
cadena = "Hola mundo"
cadena = cadena:gsub("o", "")
print(cadena)
```

La salida sería:

```Lua
Hla mund
```

En este ejemplo, hemos eliminado todas las "o" de "Hola mundo" utilizando `gsub()`.

## A Fondo

La función `gsub()` es parte intrínseca del lenguaje Lua desde sus primeras versiones. Su nombre proviene del inglés "Global Substitution", o "Sustitución Global" en español.

Existen otras maneras de eliminar caracteres coincidentes con un patrón en Lua, como con el uso de iteradores en combinación con `string.match()` o `string.gmatch()`, pero `gsub()` tiene la ventaja de ser simple y directa.

Debajo del capó, `gsub()` utiliza las expresiones regulares de Lua, que son muy poderosas pero también pueden ser complejas para los recién llegados. Es importante recordar que `gsub()` siempre devuelve una nueva cadena, ya que las cadenas en Lua son inmutables.

## Ver También

- Referencia de Lua string library: https://www.lua.org/pil/20.html
- Tutorial en español sobre manipulación de cadenas de caracteres en Lua: http://tutorialspoint.com/lua/lua_strings.htm