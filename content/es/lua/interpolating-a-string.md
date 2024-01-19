---
title:                "Interpolando una cadena de texto"
html_title:           "Haskell: Interpolando una cadena de texto"
simple_title:         "Interpolando una cadena de texto"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
La interpolación de strings en Lua nos permite insertar variables en cadenas de texto. Los programadores la utilizan para simplificar la programación y hacerlo más legible y fácil de mantener.

## Cómo:
En Lua, usamos la función `format` de la biblioteca `string` para   interpolación. Veamos un ejemplo:

```Lua
nombre = "Juan"
print(string.format("Hola, %s!", nombre)) 
-- Salida: Hola, Juan!
```
En este código, `%s` es un marcador de posición que se sustituye por la variable `nombre`.

## Buceo Profundo
Históricamente, Lua no ofrecía la función `format` en sus primeras versiones, haciendo que la concatenación de strings fuera más tediosa. Con la implementación de `format`, poder introducir variables en las cadenas de texto se volvió mucho más sencillo.
Alternativamente, puedes usar concatenación de strings para lograr el mismo resultado, pero carece de la simplicidad y limpieza que proporciona la interpolación.
El detalle de implementación específico de `format` en Lua implica la sustitución de las especificaciones de formato (por ejemplo, `%s` para strings) en la cadena original por los argumentos especificados.

## Ver también
El [Manual de Referencia de Lua](https://www.lua.org/manual/5.4/manual.html) proporciona una visión más detallada de la función `format` y las especificaciones de formato.
Aquí puedes leer mas articulos sobre programación en Lua [http://www.lua.org/articles.html](http://www.lua.org/articles.html).