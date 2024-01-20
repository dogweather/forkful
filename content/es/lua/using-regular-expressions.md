---
title:                "Usando expresiones regulares"
html_title:           "Go: Usando expresiones regulares"
simple_title:         "Usando expresiones regulares"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Expresiones regulares en Lua

## ¿Qué y por qué?

Las expresiones regulares son patrones que se utilizan para encontrar ciertas combinaciones de caracteres dentro de una cadena de texto. Los programadores las usan porque pueden simplificar la tarea de buscar, dividir y reemplazar texto.

## Cómo hacerlo

En Lua, utilizamos la biblioteca `string` para trabajar con expresiones regulares. Aquí te dejamos un ejemplo de cómo encontrar y reemplazar texto.

```Lua
text = "¡Hola, mundo!"
print(string.gsub(text, "mundo", "programador"))
```

Salida:

```
¡Hola, programador!
```

Y aquí un ejemplo de cómo dividir un texto en palabras usando la función `gmatch`.

```Lua
text = "programación en Lua"
for word in string.gmatch(text, "%a+") do
   print(word)
end
```

Salida:

```
programación
en
Lua
```

## Profundización

Las expresiones regulares se utilizan desde los inicios de las ciencias de la computación. Si bien Lua no ofrece soporte completo para expresiones regulares como Perl o Python, su conjunto de funciones en la biblioteca `string` es suficiente para tareas básicas de procesamiento de texto.

Existen bibliotecas de terceros como `lrexlib` y `lua-aho-corasick` que ofrecen una mayor funcionalidad si necesitas un análisis más complejo.

La implementación de las expresiones regulares en Lua es más eficiente en términos de memoria y rendimiento en comparación con otros lenguajes de scripting, lo cual hace de Lua una elección sólida para el procesamiento de texto en aplicaciones con recursos limitados.

## Ver también

Puedes aprender más sobre el manejo de cadenas de caracteres y expresiones regulares en Lua en los siguientes recursos:

- [Manual de referencia de Lua](http://www.lua.org/manual/5.3/)
- [Programación en Lua](http://www.lua.org/pil/)
- [Biblioteca de cadenas de Lua](http://lua-users.org/wiki/StringLibraryTutorial)