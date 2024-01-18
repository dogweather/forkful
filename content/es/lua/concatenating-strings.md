---
title:                "Uniendo cadenas"
html_title:           "Lua: Uniendo cadenas"
simple_title:         "Uniendo cadenas"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Concatenar cadenas de texto, en palabras simples, es unir dos o más palabras o frases para crear una nueva. Los programadores a menudo usan esta técnica para crear mensajes o información personalizada para sus programas.

## ¿Cómo hacerlo?
La concatenación de cadenas de texto en Lua es muy sencilla. Solo necesitas usar el operador ".." para unir las cadenas deseadas. Aquí hay un ejemplo: 
```Lua 
local primera_cadena = "¡Hola"
local segunda_cadena = "mundo!"

print(primera_cadena .. " " .. segunda_cadena) -- Output: ¡Hola mundo!
```

También puedes usar esta técnica para crear cadenas más largas, simplemente tienes que ir agregando más cadenas separadas por el operador "..". Aquí hay otro ejemplo: 
```Lua 
print("Soy un " .. "programador " .. "y me encanta " .. "Lua") -- Output: Soy un programador y me encanta Lua
```

## Inmersión profunda
La concatenación de cadenas de texto es una técnica común en la programación y es utilizada en varios lenguajes de programación. En Lua, puedes utilizar la función "string.format()" para imprimir cadenas de una manera más estructurada. 

También existen otras formas de unir cadenas de texto, como utilizar arreglos de cadenas o incluso utilizar la función "table.concat()" para unir cadenas almacenadas en una tabla. Sin embargo, utilizar el operador ".." sigue siendo la forma más sencilla y común de hacerlo.

Es importante tener en cuenta que cuando concatenas cadenas, todas las variables deben ser del tipo cadena de texto. En caso de que alguna de ellas sea de otro tipo, Lua intentará convertirla a cadena, pero es posible que el resultado no sea el esperado.

## Ver también
Si quieres profundizar más en el tema, puedes revisar la documentación oficial de Lua para obtener más detalles sobre la concatenación de cadenas de texto. También puedes explorar otras formas de trabajar con cadenas en el lenguaje, como las funciones de búsqueda y manipulación de cadenas.