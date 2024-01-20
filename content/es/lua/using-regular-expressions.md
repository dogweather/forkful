---
title:                "Uso de expresiones regulares"
html_title:           "Arduino: Uso de expresiones regulares"
simple_title:         "Uso de expresiones regulares"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qué & Por Qué?
Las expresiones regulares son patrones que se usan para encontrar y manipular texto. Los programadores las utilizan porque son herramientas poderosas y eficientes para validar, buscar, y reemplazar secuencias de caracteres dentro de strings.

## Cómo hacerlo:
```Lua
local texto = "Hola, mi número es 123-456-7890."
-- Busca un número de teléfono
local patron = "%d%d%d%-%d%d%d%-%d%d%d%d"
print(texto:match(patron))  -- Output: 123-456-7890

-- Reemplaza números de teléfono con 'PRIVADO'
local texto_modificado = texto:gsub(patron, "PRIVADO")
print(texto_modificado)  -- Output: Hola, mi número es PRIVADO.
```

## Inmersión Profunda
Las expresiones regulares en Lua tienen su origen en las herramientas de manipulación de texto UNIX, pero son más simples y limitadas en funcionalidad. Alternativas como PCRE (Perl Compatible Regular Expressions) ofrecen una riqueza mayor en expresiones complejas, pero para su uso en Lua, se requieren bibliotecas adicionales. A nivel de implementación, Lua maneja las expresiones regulares a través de su propia librería de patrones, la cual difiere de la sintaxis POSIX o Perl típica.

## Ver También
- [Referencia de Patrones de Lua](https://www.lua.org/manual/5.4/manual.html#6.4.1)
- [Tutorial en línea de Lua](https://www.lua.org/pil/20.2.html)
- [Wikilibros sobre Expresiones regulares](https://en.wikibooks.org/wiki/Regular_Expressions)