---
title:                "Interpolación de cadenas de texto"
date:                  2024-01-20T17:51:21.906454-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolación de cadenas de texto"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

La interpolación de cadenas permite insertar variables o expresiones dentro de una cadena de texto. Los programadores la utilizan para crear strings dinámicos que incluyan valores cambiantes sin tener que concatenar explícitamente.

## Cómo se hace:

```Lua
local nombre = "Mundo"
local mensaje = ("Hola, %s!"):format(nombre)
print(mensaje)  -- Salida: Hola, Mundo!
```

Otra forma es con la función `string.format()`:

```Lua
local temperatura = 25
local info = string.format("La temperatura actual es de %d grados.", temperatura)
print(info)  -- Salida: La temperatura actual es de 25 grados.
```

En Lua 5.4, se puede hacer de manera más directa:

```Lua
local usuario = "Ana"
local puntos = 30
print(f"Hola {usuario}, tienes {puntos} puntos.")  -- Salida: Hola Ana, tienes 30 puntos.
```

## Profundización

Históricamente, en Lua se usaba la concatenación con `..` o la función `string.format()` para incorporar variables en cadenas. A diferencia de otros lenguajes que ofrecen interpolación de cadenas de forma nativa con una sintaxis más simplificada, Lua ha añadido esta funcionalidad recientemente, en la versión 5.4, con una sintaxis similar a la de Python y JavaScript.

Como alternativa a la interpolación, se podía hacer uso de `table.concat()` para unir piezas de un array en una cadena, pero esto no es muy práctico para la inserción de variables simples.

En la implementación, la interpolación nativa en Lua utiliza 'f-strings', que deben ser precedidos por una `f` antes de las comillas de la cadena. Las expresiones dentro de las llaves `{}` son evaluadas y convertidas en cadenas, siendo luego insertadas en el lugar correspondiente.

## Ver También

- Referencia oficial de Lua 5.4: [https://www.lua.org/manual/5.4/](https://www.lua.org/manual/5.4/)
- "Programming in Lua" (cuarta edición) para profundizar en el lenguaje: [https://www.lua.org/pil/contents.html](https://www.lua.org/pil/contents.html)
