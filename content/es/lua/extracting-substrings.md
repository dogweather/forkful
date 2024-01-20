---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?

Extraer subcadenas en programación significa sacar una cadena menor de una cadena mayor. Los programadores hacen esto para manipular y evaluar datos más fácilmente.

## Cómo hacerlo:

Aquí están algunas formas de extraer subcadenas en Lua:

```Lua
cadena = "Hola, Mundo Lua"

print(string.sub(cadena, 7)) -- salida: Mundo Lua
print(string.sub(cadena, 7, 11)) -- salida: Mundo
print(string.sub(cadena, -3, -1)) -- salida: Lua
```

En estas muestras, `string.sub` toma tres argumentos: la cadena original, el índice inicial, y el índice final al cual se debe extraer la subcadena. El índice final es opcional; si no se suministra, la función devolverá la subcadena hasta el final de la cadena.

## Inmersión profunda

Historicamente, Lua es conocida por su eficiencia y ligereza. Sus funciones para manipulación de cadenas son bastante poderosas, entre ellas, extractar subcadenas. Lua proporciona `string.sub`, pero también una gama de otros métodos como `string.match`, que puede ser una alternativa dependiendo del contexto.

A nivel de implementación, Lua esconde los detalles de cómo exactamente se extraen estas subcadenas. Pero vale la pena señalar que aunque la indexación en Lua generalmente comienza desde 1, puedes usar índices negativos con `string.sub`. Un índice negativo se cuenta desde el final de la cadena, por ejemplo, -1 es el último caracter.

## Ver También

Para aprender más sobre cadenas y programación de Lua, puedes revisar los siguientes enlaces:

- Manual de Referencia de Lua: https://www.lua.org/manual/5.3/manual.html#6.4
- Cadenas en Lua: https://www.tutorialspoint.com/lua/lua_strings.htm