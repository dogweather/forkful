---
title:                "Escritura de un archivo de texto"
date:                  2024-01-19
html_title:           "Bash: Escritura de un archivo de texto"
simple_title:         "Escritura de un archivo de texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir en un archivo de texto permite guardar datos para usar en el futuro. Programadores lo hacen para mantener registros, guardar estados de un programa, o facilitar la interacción con otros sistemas.

## Cómo Hacerlo:
En Haskell, usamos la función `writeFile` del módulo `System.IO` para escribir en archivos.

```Haskell
import System.IO

main :: IO ()
main = do
    let str = "Hola, este es un archivo de texto!" -- Texto para escribir
    writeFile "ejemplo.txt" str -- Crea o sobrescribe el archivo "ejemplo.txt" con el texto
    putStrLn "Archivo escrito con éxito!"
```

La ejecución del programa creará o sobrescribirá el archivo `ejemplo.txt` con el texto indicado.

## Inmersión Profunda
Historicamente, la lectura y escritura de archivos en Haskell necesitaba manejar explícitamente los manejar de archivos (`Handle`), pero `writeFile` simplifica el proceso. Alternativamente, `appendFile` añade texto al final de un archivo existente. Detrás de escenas, `writeFile` abre el archivo, escribe el contenido y lo cierra automáticamente.

## Ver También
- [Haskell `System.IO` documentation](https://hackage.haskell.org/package/base-4.16.1.0/docs/System-IO.html)
