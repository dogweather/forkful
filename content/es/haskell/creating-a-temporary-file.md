---
title:                "Haskell: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué crear un archivo temporal en Haskell

Crear un archivo temporal es una práctica común en programación. Puede ser útil cuando necesitamos almacenar datos temporales antes de guardarlos permanentemente o cuando trabajamos con archivos grandes y no queremos saturar el disco duro. En este artículo, veremos cómo crear y manejar archivos temporales en Haskell.

## Cómo hacerlo

En Haskell, podemos crear un archivo temporal utilizando la función `withTempFile` del módulo `System.IO.Temp`. Esta función toma dos argumentos: la ubicación donde se creará el archivo temporal y una función que manejará el archivo. La función `withTempFile` creará y abrirá el archivo temporal, ejecutará la función dada y, una vez finalizada, eliminará automáticamente el archivo temporal.

Veamos un ejemplo de cómo podríamos utilizar esta función para crear un archivo temporal y escribir en él:

```Haskell
import System.IO.Temp (withTempFile)
import System.IO (hPutStrLn, hClose)

main = withTempFile "." $ \tmpFilePath tmpFileHandle -> do
    hPutStrLn tmpFileHandle "¡Hola, mundo!"
    hClose tmpFileHandle
```

En este ejemplo, utilizamos la función `withTempFile` para crear un archivo temporal en el directorio actual (`.`). Luego, utilizamos el manejador de archivo `tmpFileHandle` para escribir el texto "¡Hola, mundo!" en el archivo y finalmente lo cerramos con `hClose`.

Podemos ejecutar este código y verificar que se haya creado el archivo temporal correctamente y que contenga el texto que escribimos.

## Profundizando en la creación de archivos temporales

Si deseamos tener más control sobre la creación y eliminación de archivos temporales, podemos utilizar las funciones `openTempFile` y `removeFile` del módulo `System.IO`. La función `openTempFile` toma como argumentos la ubicación y el prefijo del archivo temporal, y devuelve tanto el nombre como el manejador del archivo creado. Por otro lado, la función `removeFile` elimina un archivo dado su nombre.

A continuación, un ejemplo de cómo utilizar estas dos funciones:

```Haskell
import System.IO (openTempFile, removeFile, hPutStrLn, hClose)

main = do
    (tmpFilePath, tmpFileHandle)  <- openTempFile "." "temp-"
    hPutStrLn tmpFileHandle "¡Hola, mundo!"
    hClose tmpFileHandle
    removeFile tmpFilePath
```

Aquí, utilizamos `openTempFile` para crear un archivo temporal con el prefijo "temp-" en el directorio actual. Luego, escribimos en el archivo utilizando el manejador `tmpFileHandle`, lo cerramos y finalmente eliminamos el archivo utilizando su nombre `tmpFilePath`.

## Ver también

- Documentación oficial de `System.IO.Temp`: https://hackage.haskell.org/package/temporary/docs/System-IO-Temp.html
- Guía de Haskell sobre creación de archivos temporales: https://wiki.haskell.org/Create_temporary_files