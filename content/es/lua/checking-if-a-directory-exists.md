---
title:                "Verificando si un directorio existe"
html_title:           "Lua: Verificando si un directorio existe"
simple_title:         "Verificando si un directorio existe"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Comprobar si un directorio existe es una práctica común en programación para evitar errores al intentar acceder a un directorio inexistente. Los programadores lo hacen para asegurar que el flujo de la aplicación sea correcto y en caso necesario, tomar medidas.

## Cómo realizarlo:

En Lua, puedes comprobar si un directorio existe usando la función `lfs.attributes`. Aquí te muestro un ejemplo:

```Lua
lfs = require('lfs')

-- Función para comprobar si un directorio existe
function dir_exists(dir)
  -- Comprueba si el atributo "mode" del directorio es "directory"
  return lfs.attributes(dir, 'mode') == 'directory'
end

-- Usar la función en un directorio
if dir_exists('/ruta/a/directorio/') then
  print('El directorio existe.')
else
  print('El directorio no existe.')
end
```

Al ejecutar este script, verás uno de los siguientes mensajes en la consola:

```
El directorio existe.
```

o

```
El directorio no existe.
```

## Más detalles:

La función `lfs.attributes` es parte de LuaFileSystem, un módulo Lua para manejo de archivos y directorios que es compatible con sistemas operativos UNIX y Windows. Antes de que se estandarizara este módulo, los programadores tenían que usar una mezcla de llamadas a funciones del sistema y otras bibliotecas para lograr la misma tarea.

Alternativamente, se podría usar la función `os.execute` con el comando `test -d`, pero esto no es recomendable ya que depende del sistema operativo y podría causar problemas con la portabilidad del código.

La funcionalidad subyacente para comprobar si un directorio existe en Lua realmente viene del sistema operativo subyacente. El módulo LuaFileSystem actúa simplemente como un puente entre Lua y las funciones del sistema operativo.

## Ver también:

Para obtener más información y ejemplos, revisa la documentación oficial de LuaFileSystem en https://keplerproject.github.io/luafilesystem/. También te puede interesar aprender más sobre la manipulación de archivos y directorios en Lua en general, para lo cual puedes usar como punto de partida esta guía de inicio rápido: http://lua-users.org/wiki/FileSystemOperations.