---
title:                "Buscando y reemplazando texto"
html_title:           "C: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Buscar y reemplazar es una operación habitual en la programación que nos permite localizar cadenas de texto y cambiarlas por otras. Este procedimiento es muy útil para actualizar datos, corregir errores o modificar texto para adaptarlo a ciertas situaciones.

## Cómo hacerlo:
En Lua, usamos la función gsub para buscar y reemplazar texto. Aquí te muestro cómo funciona:

```Lua
texto_original = "¡Hola, mundo!"
texto_modificado = string.gsub(texto_original, "mundo", "Lua")
print(texto_modificado)
```

Este código imprimirá: "¡Hola, Lua!".

## Inmersión profunda:

1. **Contexto histórico:** La función gsub de Lua se basa en la función homónima de las expresiones regulares en Perl y otros lenguajes de programación. Su nombre viene de 'global substitution' (sustitución global).
   
2. **Alternativas:** Hay otros lenguajes de programación que ofrecen funcionalidades similares, como JavaScript con su método `replace()`, o Python con `re.sub()`.
   
3. **Detalles de implementación:** La función gsub devuelve dos valores: la cadena modificada y el número de sustituciones realizadas. También permite usar patrones de búsqueda más avanzados, al igual que funciones personalizadas para el reemplazo.

```Lua
texto_original = "Lua es genial, genial, genial!"
texto_modificado, numero_sustituciones = string.gsub(texto_original, "genial", "increíble")
print(texto_modificado)
print(numero_sustituciones)
```

Este código imprimirá: "Lua es increíble, increíble, increíble!" y "3".

## Ver también:
Para profundizar aún más en este tema, te recomiendo revisar los siguientes recursos:

- [Documentación oficial de Lua sobre gsub](http://www.lua.org/manual/5.3/manual.html#6.4.2)
- [String Manipulation en Lua (maniobra de cadenas)](https://www.tutorialspoint.com/lua/lua_strings.htm)
- [Patrones en Lua](http://lua-users.org/wiki/PatternsTutorial)