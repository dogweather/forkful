---
title:                "Creando un archivo temporal"
html_title:           "Haskell: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

Crear un archivo temporal es una tarea común en la programación. Puede ser útil para realizar pruebas, generar datos temporales o almacenar información temporalmente antes de escribirla en un archivo permanente.

## Cómo

Para crear un archivo temporal en Haskell, utilizamos la función `withSystemTempFile` de la biblioteca `System.IO.Temp`. Esta función toma dos parámetros: una función que se encargará de manipular el archivo temporal y una cadena que representa un patrón de nombre para el archivo.

```Haskell
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStrLn, hGetContents)

main :: IO ()
main = do
  withSystemTempFile "temp.txt" $ \h filepath -> do
    hPutStrLn h "Este es un archivo temporal."
    hSeek h AbsoluteSeek 0
    contents <- hGetContents h
    putStrLn contents
```

En este ejemplo, utilizamos `withSystemTempFile` para crear un archivo temporal con el patrón de nombre "temp.txt" y escribir una línea de texto en él. Luego, leemos el contenido del archivo y lo imprimimos en la consola.

El archivo temporal se eliminará automáticamente una vez que finalice la función, por lo que no es necesario preocuparse por borrarlo manualmente.

## Deep Dive

Cuando creamos un archivo temporal utilizando `withSystemTempFile`, también obtenemos un manejador de archivo (representado por `h` en el ejemplo) y una ruta de archivo (representada por `filepath`). Esto nos permite realizar cualquier operación de archivo que necesitemos antes de que el archivo sea eliminado.

Además, si no especificamos un patrón de nombre al crear el archivo temporal, se utilizará un nombre aleatorio generado por el sistema operativo. Esto puede ser útil si necesitamos crear múltiples archivos temporales en una sola ejecución del programa.

## Ver también

- [Documentación de `System.IO.Temp`](https://hackage.haskell.org/package/temp-1.3.0.4/docs/System-IO-Temp.html)
- [Guía de programación en Haskell para principiantes](https://www.haskell.org/documentation/)