---
title:                "Haskell: Creando un archivo temporal"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Por qué crear un archivo temporal en Haskell

Crear un archivo temporal puede ser una necesidad en algunas situaciones al programar en Haskell. Ya sea para realizar pruebas, almacenar datos temporales o compartir información entre diferentes funciones, saber cómo crear y trabajar con archivos temporales puede ser útil en diferentes escenarios. En este artículo, aprenderemos cómo crear y manipular archivos temporales en Haskell.

## Cómo hacerlo

Para crear un archivo temporal en Haskell, utilizaremos la librería System.Directory y su función "withSystemTempFile". En primer lugar, importaremos la librería en nuestro código:

```Haskell
import System.Directory
```

Luego, utilizaremos la función "withSystemTempFile", que toma como parámetros una cadena de texto con el patrón del nombre del archivo temporal y una función que tomará como argumento el nombre del archivo temporal creado. Por ejemplo, podemos crear un archivo temporal para guardar una lista de números aleatorios de la siguiente manera:

```Haskell
withSystemTempFile "random-numbers.txt" $ \tempFilePath tempHandle -> do
    let numbers = [5,2,8,3,9]
    hPutStrLn tempHandle (show numbers)
    hClose tempHandle
```

Aquí, estamos creando un archivo temporal llamado "random-numbers.txt" y pasando la función anónima que toma como argumentos el nombre del archivo temporal y su manejador, para escribir los números en el archivo utilizando la función "hPutStrLn". Después de finalizar la escritura, cerramos el archivo temporal con "hClose".

## Profundizando

Crear un archivo temporal en Haskell implica una serie de pasos detrás de escena. En primer lugar, se genera un nombre único para el archivo temporal utilizando el patrón proporcionado. Luego, el archivo es creado en el directorio temporal del sistema operativo. Una vez terminado el uso del archivo, éste es eliminado automáticamente.

Además, podemos utilizar la función "withSystemTempDirectory" para crear un directorio temporal en lugar de un archivo. Esta función toma como parámetros el patrón del nombre y una función que manipulará el directorio creado.

## Ver también

- Documentación de la librería System.Directory: https://hackage.haskell.org/package/directory/docs/System-Directory.html
- Tutorial de Haskell: https://www.haskell.org/tutorial/
- Ejemplos de uso de archivos temporales en Haskell: https://github.com/vincente923/Manipulating-Files-in-Haskell/tree/master/TmpFile