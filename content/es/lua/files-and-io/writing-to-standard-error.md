---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:41.656539-07:00
description: "Escribir en el error est\xE1ndar (stderr) se trata de dirigir mensajes\
  \ de error y salidas de diagn\xF3stico a un canal separado, distinto de la salida\
  \ est\xE1ndar\u2026"
lastmod: '2024-03-11T00:14:33.038710-06:00'
model: gpt-4-0125-preview
summary: "Escribir en el error est\xE1ndar (stderr) se trata de dirigir mensajes de\
  \ error y salidas de diagn\xF3stico a un canal separado, distinto de la salida est\xE1\
  ndar\u2026"
title: "Escribiendo en el error est\xE1ndar"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Escribir en el error estándar (stderr) se trata de dirigir mensajes de error y salidas de diagnóstico a un canal separado, distinto de la salida estándar (stdout). Los programadores hacen esto para diferenciar los resultados regulares del programa de la información de error, simplificando los procesos de depuración y registro.

## Cómo hacerlo:
En Lua, se puede lograr escribir en stderr utilizando la función `io.stderr:write()`. Así es como puedes escribir un simple mensaje de error en el error estándar:

```lua
io.stderr:write("Error: Entrada inválida.\n")
```

Si necesitas sacar una variable o combinar múltiples piezas de datos, concaténalas dentro de la función write:

```lua
local errorMessage = "Entrada inválida."
io.stderr:write("Error: " .. errorMessage .. "\n")
```

**Salida de muestra en stderr:**
```
Error: Entrada inválida.
```

Para escenarios más complejos, o cuando se trabaja con aplicaciones más grandes, podrías considerar bibliotecas de registro de terceros como LuaLogging. Con LuaLogging, puedes dirigir los registros a diferentes destinos, incluyendo stderr. Aquí tienes un breve ejemplo:

Primero, asegúrate de que LuaLogging esté instalado usando LuaRocks:

```
luarocks install lualogging
```

Luego, para escribir un mensaje de error en stderr usando LuaLogging:

```lua
local logging = require("logging")
local logger = logging.stderr()
logger:error("Error: Entrada inválida.")
```

Este enfoque ofrece la ventaja de un registro estandarizado en toda tu aplicación, con la flexibilidad adicional de establecer niveles de registro (por ejemplo, ERROR, ADVERTENCIA, INFORMACIÓN) a través de una API sencilla.
