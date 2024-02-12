---
title:                "Concatenación de cadenas de texto"
aliases: - /es/lua/concatenating-strings.md
date:                  2024-01-20T17:35:09.347259-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenación de cadenas de texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qué es y por qué?
La concatenación de cadenas es simplemente pegar una cadena de texto al final de otra. Los programadores lo hacen para construir mensajes, combinar datos y generar salida dinámica.

## Cómo hacerlo:
En Lua, concatenas cadenas con el operador `..`. Es fácil y directo.

```Lua
local saludo = "Hola"
local mundo = "mundo"
local fraseCompleta = saludo .. " " .. mundo
print(fraseCompleta)  -- Salida: Hola mundo
```

Si necesitas incluir números, conviértelos a cadenas primero con `tostring`.

```Lua
local temperatura = 25
print("La temperatura es " .. tostring(temperatura) .. "°C")
-- Salida: La temperatura es 25°C
```

## Análisis Profundo:
La concatenación de cadenas es una operación básica en Lua introducida desde sus inicios. En versiones muy antiguas de Lua, la concatenación se podía hacer con la función `..`, la cual aún prevalece por su simplicidad. También se puede utilizar la función `table.concat` para concatenar un arreglo de cadenas, lo cual es más eficiente para un gran número de elementos. Internamente, Lua gestiona la memoria de las cadenas de manera que se optimiza el proceso de concatenación, aunque en grandes volúmenes puede impactar en el rendimiento.

```Lua
local frases = {"Hola", "mundo", "con", "Lua"}
local textoCompleto = table.concat(frases, " ")
print(textoCompleto)  -- Salida: Hola mundo con Lua
```

## Ver También:
Para más detalles sobre la concatenación y manejo de cadenas en Lua, revisa:
- [Lua Reference Manual: Strings](https://www.lua.org/manual/5.4/manual.html#6.4)
- [Programming in Lua: Strings](https://www.lua.org/pil/21.2.html)
