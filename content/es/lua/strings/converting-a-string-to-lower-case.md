---
date: 2024-01-20 17:38:46.715421-07:00
description: "Convertir una cadena a min\xFAsculas es cambiar todos los caracteres\
  \ alfab\xE9ticos de una cadena a su forma en min\xFAscula. Los programadores lo\
  \ hacen para\u2026"
lastmod: '2024-02-25T18:49:55.660472-07:00'
model: gpt-4-1106-preview
summary: "Convertir una cadena a min\xFAsculas es cambiar todos los caracteres alfab\xE9\
  ticos de una cadena a su forma en min\xFAscula. Los programadores lo hacen para\u2026"
title: "Conversi\xF3n de una cadena de texto a min\xFAsculas"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Convertir una cadena a minúsculas es cambiar todos los caracteres alfabéticos de una cadena a su forma en minúscula. Los programadores lo hacen para unificar texto, por ejemplo, para comparar cadenas sin importar el caso original.

## Cómo hacerlo:
Lua hace que convertir a minúsculas sea coser y cantar. Usamos la función `string.lower()`. Aquí un par de ejemplos:

```Lua
local textoOriginal = "Hola Mundo!"
local textoEnMinusculas = string.lower(textoOriginal)
print(textoEnMinusculas)  -- imprime: hola mundo!
```

Si tienes varias cadenas, simplemente repite el proceso:

```Lua
local saludos = {"Hola Amigos", "Buenos DÍAS", "Buenas TARDES"}
for i, saludo in ipairs(saludos) do
    saludos[i] = string.lower(saludo)
end
print(table.concat(saludos, ", "))  -- imprime: hola amigos, buenos días, buenas tardes
```

## Profundizando

Antes de que las computadoras tuvieran soporte internacional, lidiar con minúsculas y mayúsculas era más sencillo. Ahora, con Unicode y varios idiomas, es un poco más complejo. En Lua, `string.lower()` hace el trabajo para ti en la mayoría de los idiomas, pero no maneja excepciones locales específicas (como caracteres turcos especiales).

Si no te sirve `string.lower()`, puedes buscar o construir una función de conversión basada en las reglas específicas de tu idioma o situación. Alternativamente, la biblioteca de terceros `utf8` (Lua 5.3 en adelante) puede manejar casos Unicode más complicados.

La función corre así: recorre la cadena de texto y convierte cada carácter Unicode de una letra a su versión minúscula. No cambia números, símbolos o caracteres que no tienen representación en minúscula.

## Ver También

- La referencia oficial de la función `string.lower()`: https://www.lua.org/manual/5.4/manual.html#pdf-string.lower
- Documentación sobre la codificación de caracteres Unicode en Lua: http://www.unicode.org/
- Un foro de Lua donde los programadores discuten sobre manipulación de texto y cuestiones de internacionalización: https://www.lua.org/wshop18/Ierusalimschy.pdf
