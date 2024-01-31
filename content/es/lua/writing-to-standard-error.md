---
title:                "Escribiendo en el error estándar"
date:                  2024-01-19
html_title:           "Arduino: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"

category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Escribir en el error estándar (stderr) permite separar la información de error de la salida normal. Los programadores usan esto para depurar y registrar errores sin interrumpir el flujo regular.

## Cómo hacerlo:
```Lua
-- Para enviar un mensaje de error
io.stderr:write("Ha ocurrido un error\n")

-- Ejemplo más complejo con assert
local archivo, err = io.open("archivo_inexistente.txt", "r")
assert(archivo, "Error al abrir el archivo: " .. tostring(err))
```

Salida de muestra al ejecutar el código anterior (suponiendo que el archivo no existe):
```
Error al abrir el archivo: archivo_inexistente.txt: No such file or directory
```

## Análisis Profundo:
Antes, en versiones antiguas de Lua, `io.stderr` no existía y se usaban otros métodos como `os.execute()` para manejar los errores. Hoy en día, `io.stderr` es la forma estándar y directa de escribir en stderr. A diferencia de la salida estándar (stdout), lo escrito en stderr se muestra incluso cuando stdout es redirigido.

## Ver También:
- Documentación oficial de Lua: [io library](https://www.lua.org/manual/5.4/manual.html#6.8)
- Guía sobre manejo de errores en Lua: [Programming in Lua (Fourth edition)](https://www.lua.org/pil/contents.html)
