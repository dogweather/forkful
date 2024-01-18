---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Lua: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

#¿Qué y por qué?
En Lua, eliminar caracteres que coinciden con un patrón es una operación común en programación que consiste en eliminar ciertos caracteres específicos de una cadena de texto. Los programadores suelen realizar esta acción para limpiar datos o manipular cadenas de texto de manera más eficiente.

#Cómo hacerlo:
A continuación, se presentan dos ejemplos de cómo se puede eliminar caracteres que coinciden con un patrón en Lua:

```Lua
-- Ejemplo 1: Eliminando vocales de una cadena de texto
local texto = "Hola mundo"
texto = texto:gsub("[aeiouAEIOU]", "")
print(texto)
-- Output: Hl mnd

-- Ejemplo 2: Eliminando dígitos de una cadena de texto
local texto = "1,2,3,4,5,6"
texto = texto:gsub("%d", "")
print(texto)
-- Output: ,,,
```

En el primer ejemplo, se utiliza la función `gsub` para reemplazar todas las vocales (tanto mayúsculas como minúsculas) con una cadena vacía, eliminándolas de la cadena original. En el segundo ejemplo, se utiliza `%d` para representar cualquier dígito y también se reemplaza con una cadena vacía.

#Profundizando:
##Contexto histórico
El concepto de patrones y la función `gsub` en Lua se basan en la herramienta de línea de comandos `sed` utilizada en sistemas Unix. Esta herramienta permitía realizar varias operaciones en cadenas de texto, incluyendo la eliminación de caracteres que coincidían con un patrón.

##Alternativas
Además de `gsub`, Lua ofrece otras funciones para manipular cadenas de texto, como `sub` para sustituir una sola ocurrencia de un patrón, `match` para obtener una subcadena que coincide con un patrón y `find` para encontrar la posición de una subcadena que coincide con un patrón.

También existen otras formas de eliminar caracteres que coinciden con un patrón, como utilizar expresiones regulares en Lua o utilizar herramientas externas como `awk` o `tr`.

##Detalles de implementación
La función `gsub` acepta un tercer argumento opcional que indica el número máximo de reemplazos que se realizarán. También es posible utilizar variables en los patrones para hacerlos más dinámicos.

#Ver también:
- Documentación oficial de Lua sobre la función `gsub`: https://www.lua.org/manual/5.4/manual.html#pdf-string.gsub 
- Ejemplos de uso de `gsub` en la comunidad: https://stackoverflow.com/questions/19587241/using-gsub-to-remove-all-the-unwanted-characters-from-a-line