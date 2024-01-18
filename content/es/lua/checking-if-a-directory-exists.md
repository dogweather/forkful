---
title:                "Comprobando si existe un directorio."
html_title:           "Lua: Comprobando si existe un directorio."
simple_title:         "Comprobando si existe un directorio."
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Comprobar si un directorio existe es una acción común que los programadores realizan en Lua. Esta verificación es necesaria para asegurarse de que un directorio necesario para el programa esté presente y accesible. También puede ser útil en situaciones donde se desea manipular los archivos dentro de un directorio específico.

## Cómo hacerlo:
```Lua
-- Verificar si un directorio existe
if lfs.attributes("ruta_del_directorio", "mode") == "directory" then
  print("El directorio existe")
else
  print("El directorio no existe")
end

-- Otra forma de verificar
if io.open("ruta_del_directorio") then
  print("El directorio existe")
else
  print("El directorio no existe")
end
```

## Profundizando:
En Lua, hay diferentes formas de verificar si un directorio existe. En versiones anteriores de Lua, se utilizaba la biblioteca "lfs" (sistema de archivos de Lua) para realizar esta verificación. Sin embargo, a partir de Lua 5.3, se puede utilizar la función "io.open()" para este propósito.

## Ver también:
- [Documentación oficial de Lua sobre la función io.open()](https://www.lua.org/pil/21.2.html)
- [Página de la biblioteca lfs en LuaRocks](https://luarocks.org/modules/luarocks/lfs)