---
title:                "Trabajando con JSON"
html_title:           "Lua: Trabajando con JSON"
simple_title:         "Trabajando con JSON"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Trabajar con JSON es una tarea común en la programación moderna. JSON, o JavaScript Object Notation, es un formato ligero para almacenar y transmitir datos. Los programadores lo usan porque es fácil de leer y escribir en comparación con otros formatos de datos, como XML. Además, JSON es ampliamente utilizado en aplicaciones web y móviles, por lo que es importante para los programadores aprender cómo trabajar con él.

## ¿Cómo hacerlo?

En Lua, hay una biblioteca estándar llamada "cjson" que nos permite trabajar con JSON de forma sencilla. Podemos usarla para convertir una tabla Lua en una cadena JSON y viceversa. Aquí hay un ejemplo de cómo hacerlo:

```lua
-- Convertir una tabla Lua a una cadena JSON
local t = {nombre = "Juan", edad = 25, ciudad = "Madrid"}
local json = require("cjson")
local jsonString = json.encode(t)
print(jsonString)
-- Salida: {"ciudad":"Madrid","edad":25,"nombre":"Juan"}

-- Convertir una cadena JSON a una tabla Lua
local jsonString = '{"ciudad":"Madrid","edad":25,"nombre":"Juan"}'
local t = json.decode(jsonString)
print(t.nombre)
-- Salida: Juan
```

Como se puede ver en el ejemplo, podemos usar las funciones "encode" y "decode" de la biblioteca "cjson" para convertir entre tablas Lua y cadenas JSON.

## Profundizando

JSON fue creado originalmente por Douglas Crockford en 2001, como una alternativa más sencilla a XML. Aunque todavía se utiliza XML en ciertas aplicaciones, JSON se ha convertido en el formato preferido para el intercambio de datos en aplicaciones web y móviles.

Además de la biblioteca estándar "cjson", existen otras opciones para trabajar con JSON en Lua, como "dkjson" y "lua-json". Sin embargo, "cjson" es la más utilizada debido a su rendimiento y funcionalidad.

La implementación estándar de Lua no incluye "cjson" por defecto, pero se puede instalar fácilmente con la ayuda de un gestor de paquetes como "luarocks". Solo necesitas ejecutar el siguiente comando en la terminal:

```bash
luarocks install lua-cjson
```

## Ver también

- [Tutorial de cjson en Lua](https://www.tutorialspoint.com/lua/lua_json_cjson.htm)
- [Documentación oficial de cjson](http://www.kyne.com.au/~mark/software/lua-cjson-manual.html)
- [Otras opciones para trabajar con JSON en Lua](https://stackify.com/json-in-lua/)