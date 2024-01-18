---
title:                "Encontrando la longitud de una cadena"
html_title:           "Lua: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
En programación, encontrar la longitud de una cadena es una tarea común y útil. La longitud de una cadena se refiere al número de caracteres que contiene. Los programadores a menudo necesitan conocer la longitud de una cadena para realizar operaciones como la validación de entradas de usuario o el formateo de texto.

## Cómo:
- Para encontrar la longitud de una cadena en Lua, se puede utilizar la función ```string.len()```. Esta función toma una cadena como argumento y devuelve la longitud como un número.
```
Lua
nombre = "Juan"
print(string.len(nombre))
-- Output: 4
```
- También se puede usar el operador de longitud ```#``` en una cadena para obtener su longitud. Sin embargo, este operador solo cuenta los caracteres hasta el primer carácter nulo, por lo que podría no ser preciso en caso de cadenas con caracteres nulos en medio.
```
Lua
nombre = "Juan"
print(#nombre)
-- Output: 4
```

## Inmersión profunda:
- En el pasado, muchas implementaciones de lenguajes de programación no incluían una función para encontrar la longitud de una cadena. En su lugar, los programadores tenían que recorrer manualmente cada carácter de la cadena y contarlo. Por suerte, la mayoría de los lenguajes modernos, incluyendo Lua, proporcionan una función incorporada para esta tarea.
- Además de la función ```string.len()``` y el operador ```#```, también se pueden usar otras funciones en Lua para obtener la longitud de una cadena. Por ejemplo, la función ```string.gmatch()``` puede ser utilizada para iterar sobre los caracteres de una cadena y contarlos.
- Internamente, la función ```string.len()``` utiliza la biblioteca estándar de C para obtener la longitud de una cadena. En otras palabras, está diseñada para ser eficiente y rápida en comparación con un método hecho a mano.

## Ver también:
- [Documentación de Lua para string.len()](https://www.lua.org/manual/5.4/manual.html#pdf-string.len)
- [Uso del operador # en Lua](https://lua.org/pil/3.3.html)
- [Métodos alternativos para obtener la longitud de una cadena en Lua](https://stackoverflow.com/questions/9133051/lua-length-of-string#9133090)