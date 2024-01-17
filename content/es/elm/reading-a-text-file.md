---
title:                "Leyendo un archivo de texto"
html_title:           "Elm: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¡Qué y Por qué!

¿Qué es leer un archivo de texto? Es un proceso en el que un programa toma un archivo de texto y lo convierte en datos que puede manejar y utilizar. Los programadores hacen esto para poder leer, analizar y manipular grandes cantidades de información de manera eficiente.

## ¡Cómo hacerlo!

```Elm
module Main exposing (main)

import File exposing (read)
import String

type Msg
    = FileLoaded (Result String String)

fileLoaded : Result String String -> String
fileLoaded result =
    case result of
        Ok contents ->
            contents

        Err message ->
            "No se pudo cargar el archivo. Error: " ++ message

-- Función para abrir un archivo de texto y leer su contenido
openFile : String -> Cmd Msg
openFile fileName =
    read fileName fileLoaded

-- Uso de la función para cargar el contenido del archivo "datos.txt"
main : Program Never String Msg
main =
    openFile "datos.txt"
```

## Inmersión Profunda

Leer archivos de texto ha sido una habilidad esencial para los programadores desde los primeros días de la informática. Sin embargo, hay alternativas como la lectura de datos de la web o la utilización de bases de datos para manejar grandes cantidades de información. En Elm, la función `read` se comunica con el navegador para leer el archivo, lo que puede ser una gran ventaja para los programadores web.

## Mira También

- [Documentación de `File` en la biblioteca estándar de Elm](https://package.elm-lang.org/packages/elm/file/latest/File)
- [Cómo trabajar con archivos en Elm - video tutorial](https://www.youtube.com/watch?v=eVTtC_GyEko)