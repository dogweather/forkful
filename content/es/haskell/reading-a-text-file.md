---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Leer un archivo de texto significa obtener información desde un archivo guardado en disco. Los programadores lo hacen para manipular datos persistentes y para interactuar con sistemas de archivos.

## ¿Cómo hacerlo?
La lectura de archivos en Haskell se puede hacer con funciones predefinidas. Aquí un ejemplo:

```Haskell
main = do   
    contenido <- readFile "texto.txt"
    putStrLn contenido
```
Este programa leera los contenidos de "texto.txt" y los imprimirá en la consola.

## Mirada en Detalle

Haskell, lanzado en 1990, ha tenido funciones para leer archivos desde su concepción. Se han propuesto varias alternativas a `readFile`, como `Data.ByteString` para la lectura binaria, pero aún son ampliamente utilizadas las funciones básicas.

Haskell lee archivos a través de su biblioteca de E/S recursiva. Esto significa que, a diferencia de los lenguajes imperativos que utilizan bucles para leer archivos, Haskell lee los contenidos de un archivo en un solo objetivo, lo cual puede ser más eficiente dependiendo del uso.

## Ver También
Para profundizar, les sugiero estos recursos:

1. [Haskell Wiki on File I/O](https://wiki.haskell.org/File_IO)
2. [Real World Haskell: Input and Output](http://book.realworldhaskell.org/read/io.html)
3. [Learn You a Haskell for Great Good: Chapter 7 Input and Output](http://learnyouahaskell.com/input-and-output)