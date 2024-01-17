---
title:                "Comprobando si existe un directorio"
html_title:           "Haskell: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# ¿Qué y por qué?

Comprobar si un directorio existe es una forma de verificar si un directorio específico está presente en una ubicación dada. Los programadores suelen hacerlo para asegurarse de que el código pueda acceder a un directorio antes de intentar manipular archivos dentro de él.

# Cómo hacerlo

Puedes verificar si un directorio existe usando la función `doesDirectoryExist` de la biblioteca `System.Directory` de Haskell. Esta función toma un `String` como argumento, que es la ruta al directorio que deseas verificar. El resultado será un valor booleano que indicará si el directorio existe o no.

```Haskell
import System.Directory (doesDirectoryExist)

-- Verificar si el directorio "ejemplo" existe
doesDirectoryExist "ejemplo"
-- Devuelve True si existe o False si no
```

# Inmersión profunda

Antes de la introducción de la función `doesDirectoryExist` en Haskell, los programadores tenían que usar una combinación de funciones de manejo de directorios y de archivos para verificar si un directorio existía. Esto era más verboso y requería un conocimiento más profundo del sistema de archivos.

Alternativamente, también puedes usar la función `listDirectory` para obtener una lista de todos los archivos y directorios en una ubicación específica. Luego puedes buscar en esa lista el nombre del directorio que buscas.

# Ver también

- Documentación de la función `doesDirectoryExist`: [https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:doesDirectoryExist](https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:doesDirectoryExist)
- Documentación de la función `listDirectory`: [https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:listDirectory](https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:listDirectory)