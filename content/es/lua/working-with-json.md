---
title:                "Trabajando con JSON"
html_title:           "Bash: Trabajando con JSON"
simple_title:         "Trabajando con JSON"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Trabajar con JSON significa manipular el formato de intercambio de datos JavaScript Object Notation en tus programas. Los programadores lo hacen porque JSON es ligero, fácil de leer y estándar en la web, ideal para comunicarse con APIs y almacenar configuraciones.

## Cómo Hacerlo:

Para manejar JSON en Lua, utilizamos `dkjson`, una librería popular. Aquí están los pasos básicos:

1. Instalar dkjson (a través de luarocks o descargando directamente).
2. Importar la librería en tu código.
3. Convertir datos Lua a JSON (serialización).
4. Convertir JSON a datos Lua (deserialización).

```Lua
local dkjson = require 'dkjson'

-- Serializar datos de Lua a JSON
local datos_lua = {
  nombre = "Juan",
  edad = 30,
  intereses = {"programación", "videojuegos", "viajar"}
}
local json_str = dkjson.encode(datos_lua)
print(json_str) -- {"edad":30,"intereses":["programación","videojuegos","viajar"],"nombre":"Juan"}

-- Deserializar JSON a datos de Lua
local str_json = '{"nombre":"Ana","edad":25,"intereses":["arte","lectura"]}'
local datos_json, pos, err = dkjson.decode(str_json)
if err then
  error("Error al parsear JSON: "..err)
end
print(datos_json.nombre) -- Ana
```

## Profundización:

JSON se originó en 2001, diseñado por Douglas Crockford. Hoy, es el formato estándar de intercambio de datos debido a su simplicidad y legibilidad. Hay alternativas como XML, pero JSON predomina por su eficiencia y facilidad de uso. En la implementación, considere que Lua no tiene soporte nativo para JSON, así que se necesitan librerías externas como `dkjson`, `cjson`, o `json.lua`. Estas diferencias principalmente en rendimiento y facilidades adicionales.

## Ver También:

- Documentación `dkjson`: http://dkolf.de/src/dkjson-lua.fsl/home
- LuaRocks `dkjson`: https://luarocks.org/modules/dkolf/dkjson
- JSON en la Wikipedia: https://es.wikipedia.org/wiki/JSON
- Comparativa entre JSON y XML: https://www.json.org/json-es.html
