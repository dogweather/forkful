---
title:                "Haskell: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Muchos programas y aplicaciones necesitan leer datos de archivos de texto para obtener información importante. En Haskell, esto se logra mediante la lectura de un archivo de texto y su posterior procesamiento. En esta publicación, aprenderemos cómo leer archivos de texto en Haskell y cómo utilizar esa información en nuestros programas.

## Cómo hacerlo

El primer paso para leer un archivo de texto en Haskell es importar el módulo "Data.Text.IO". Luego, podemos usar la función "readFile" para leer el contenido de un archivo de texto en una variable. Por ejemplo:

```Haskell
import Data.Text.IO as T

main :: IO ()
main = do
  archivo <- T.readFile "datos.txt"
  putStrLn archivo
```

Este código abrirá el archivo "datos.txt", lo leerá y lo almacenará en la variable "archivo". Luego, la función "putStrLn" imprimirá el contenido del archivo en la consola.

Podemos manipular el contenido del archivo de texto utilizando las funciones proporcionadas por el módulo "Data.Text". Por ejemplo, podemos separar el contenido en líneas utilizando "lines" o dividirlo en palabras utilizando "words". También podemos convertir el texto a mayúsculas o minúsculas utilizando las funciones "toUpper" y "toLower".

## Profundizando

Hasta ahora, hemos visto cómo leer el contenido de un archivo de texto y manipularlo de forma básica. Sin embargo, también existe la posibilidad de leer y procesar grandes archivos de texto de manera eficiente en Haskell.

Para archivos de gran tamaño, es recomendable utilizar la función "withFile" del módulo "System.IO". Esta función nos permite abrir el archivo en modo de lectura e ir procesando el contenido línea por línea, sin cargar todo el archivo en memoria. Por ejemplo:

```Haskell
import System.IO
import Data.Text as T

main :: IO ()
main = do
  withFile "datos.txt" ReadMode (\handle -> do
    contenido <- T.hGetContents handle
    T.putStrLn (T.toUpper contenido)
  )
```

Este código abrirá el archivo "datos.txt" en modo de lectura y luego convertirá todo el contenido a mayúsculas antes de imprimirlo en la consola. Al utilizar "withFile", podemos procesar archivos de cualquier tamaño de manera eficiente.

## Ver también

- [Documentación del módulo "Data.Text.IO"](https://hackage.haskell.org/package/text/docs/Data-Text-IO.html)
- [Documentación del módulo "System.IO"](https://hackage.haskell.org/package/base/docs/System-IO.html)
- [Tutorial de lectura de archivos en Haskell](https://www.tutorialspoint.com/haskell/haskell_file_io.htm)