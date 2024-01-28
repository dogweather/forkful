---
title:                "Eliminando comillas de una cadena"
date:                  2024-01-26T03:40:18.628987-07:00
model:                 gpt-4-0125-preview
simple_title:         "Eliminando comillas de una cadena"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Quitar las comillas de una cadena significa deshacerse de esos caracteres de comillas dobles o simples que abrazan tu texto. Los programadores hacen esto para desinfectar entradas, facilitar el análisis sintáctico o para armonizar datos que pueden estar citados de manera inconsistente.

## Cómo hacerlo:
Aquí te mostramos cómo enviar esas comillas a volar en Lua:

```lua
local function remove_quotes(str)
  return (str:gsub("^%p(.*)%p$", "%1"))
end

print(remove_quotes('"¡Hola, Mundo!"'))     -- ¡Hola, Mundo!
print(remove_quotes("'¡Adiós, Comillas!'"))  -- ¡Adiós, Comillas!
```

¡Bingo! Esas comillas desaparecieron como calcetines en una secadora.

## Análisis profundo
La gente ha estado limpiando comillas de las cadenas desde que los lenguajes podían manejar texto, que es prácticamente desde siempre. En Lua, la función `gsub` hace el trabajo pesado, utilizando patrones como un bisturí para extirpar comillas. ¿Alternativas? Claro, podrías usar expresiones regulares en lenguajes que lo soporten, o escribir tu propio bucle que mastique cada carácter (bostezo, pero oye, es tu tiempo).

El emparejamiento de patrones en Lua te da la experiencia de un regex-lite sin importar una biblioteca entera. El signo de intercalación (`^`) y el signo de dólar (`$`) coinciden con el inicio y el final de la cadena, respectivamente; `%p` coincide con cualquier carácter de puntuación. Después de deshacernos de la puntuación inicial y final, capturamos todo lo demás con `(.*),` y reemplazamos toda la coincidencia con ese grupo de captura usando `" %1"`.

Recuerda que el emparejamiento de patrones de Lua no es tan potente como los motores completos de expresiones regulares - por ejemplo, no puede contar ni retroceder. Esta simplicidad es tanto una bendición como una maldición, dependiendo de qué comillas estés manejando y dónde se estén escondiendo.

## Ver también
Sumérgete más profundamente en el emparejamiento de patrones de Lua con el libro PiL (Programación en Lua): http://www.lua.org/pil/20.2.html

Por pura elegancia, comprueba cómo lo hacen otros lenguajes para comparar, comenzando por `str.strip` de Python: https://docs.python.org/3/library/stdtypes.html#str.strip
