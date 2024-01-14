---
title:                "Haskell: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# ¿Por qué verificar si un directorio existe en Haskell?

Si estás programando en Haskell, es posible que en algún momento necesites verificar si un directorio existe. Esto puede ser útil para asegurarse de que un archivo se encuentra en la ubicación correcta, o para evitar posibles errores en el código al intentar acceder a un directorio inexistente.

# Cómo hacer la verificación

En Haskell, la forma más común de verificar si un directorio existe es utilizando la función `doesDirectoryExist` de la biblioteca `System.Directory`. Esta función toma como argumento una ruta de directorio y devuelve un valor booleano que indica si el directorio existe o no.

```Haskell
import System.Directory

-- Verifica si el directorio "miDirectorio" existe
doesDirectoryExist "miDirectorio"
```

Si el directorio existe, el resultado será `True`. Si no existe, el resultado será `False`.

# Profundizando más

Además de la función `doesDirectoryExist`, la biblioteca `System.Directory` también proporciona otras funciones útiles para trabajar con directorios, como `createDirectory` para crear un nuevo directorio o `removeDirectory` para eliminar uno existente.

También es posible utilizar la función `getDirectoryContents` para obtener una lista de todos los archivos y subdirectorios que se encuentran en un directorio específico.

# Ver también

- Documentación de `System.Directory`: https://hackage.haskell.org/package/directory/docs/System-Directory.html
- Tutorial de Haskell para principiantes: https://www.haskell.org/tutorials/learnyouahaskell.html