---
title:                "Haskell: Leyendo un archivo de texto"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué leer un archivo de texto en Haskell?

Los archivos de texto son una forma común de almacenar información en formato legible por humanos. Al aprender a leerlos en Haskell, podrás acceder y manipular datos externos en tus programas.

## Cómo hacerlo

Para leer un archivo de texto en Haskell, primero debes importar el módulo `System.IO`. Luego, puedes utilizar la función `readFile` para leer el contenido del archivo en una cadena de texto. Aquí hay un ejemplo de cómo hacerlo:

```Haskell
import System.IO

main = do
    contenido <- readFile "archivo.txt"
    putStrLn contenido
```

Este código primero lee el contenido del archivo "archivo.txt" y lo almacena en una variable llamada `contenido`. Luego, utiliza la función `putStrLn` para imprimir ese contenido en la consola. Nota que la función `readFile` también puede lanzar una excepción si el archivo no existe.

## Profundizando en la lectura de archivos de texto

La función `readFile` es útil cuando solo necesitas leer el contenido de un archivo de texto. Sin embargo, si deseas realizar operaciones más complejas, como leer línea por línea o analizar el contenido en formato CSV, deberás utilizar otras funciones y bibliotecas de Haskell.

Una función común para leer línea por línea es `readFileLines` del módulo `System.IO.Strict`. También hay bibliotecas de terceros que pueden facilitar la lectura y análisis de archivos de texto, como `Cassava` para archivos CSV o `parsec` para analizar texto con una gramática específica.

## Ver también

- [Documentación de System.IO en Haskell](https://hackage.haskell.org/package/base-4.16.0.0/docs/System-IO.html)
- [Ejemplos de lectura de archivos en Haskell](https://www.programminglogic.com/reading-files-in-haskell/)