---
title:                "Utilizando expresiones regulares"
html_title:           "Lua: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

El uso de expresiones regulares en programación se refiere a la utilización de patrones de texto para buscar y manipular cadenas de caracteres. Los programadores utilizan expresiones regulares para simplificar la tarea de búsqueda y reemplazo de un texto específico, lo cual puede ser especialmente útil en proyectos de gran escala.

## Cómo:

El siguiente ejemplo muestra cómo utilizar expresiones regulares en el lenguaje de programación Lua:

```Lua
-- Buscar una palabra específica en un texto
texto = "Este es un texto de ejemplo con la palabra clave"
palabra = "clave"
patron = "%w+" .. palabra .. "%w+"

print(string.match(texto, patron)) -- Imprime "palabra clave"

-- Reemplazar una palabra por otra en un texto
nuevo_texto = string.gsub(texto, palabra, "reemplazo")
print(nuevo_texto) -- Imprime "Este es un texto de ejemplo con la palabra reemplazo"
```

## Profundizando:

Las expresiones regulares se han utilizado en programación desde la década de 1950 y tienen su origen en la teoría de la computación y la matemática. Aunque Lua no es un lenguaje de programación especializado en expresiones regulares, proporciona una biblioteca estándar para su uso. 

Otras alternativas a las expresiones regulares incluyen el uso de funciones específicas para manipular cadenas de caracteres en lugar de patrones de texto, aunque pueden no ser tan versátiles y eficientes en ciertos casos.

La implementación de expresiones regulares en Lua se basa en la biblioteca de patrones de texto POSIX, y acepta los mismos metacaracteres que otros lenguajes de programación, como el punto para representar cualquier carácter y el asterisco para representar cero o más ocurrencias de un patrón.

## Ver también:

- [Documentación oficial de Lua sobre expresiones regulares](https://www.lua.org/manual/5.3/manual.html#6.4.1)
- [Tutorial de expresiones regulares en Lua](https://www.tutorialspoint.com/lua/lua_regular_expressions.htm)
- [Libro "Programming in Lua" (en inglés) que incluye una sección sobre expresiones regulares](https://www.lua.org/pil/20.1.html)