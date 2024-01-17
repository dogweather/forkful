---
title:                "Comprobando si existe un directorio"
html_title:           "Elm: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Comprobar si un directorio existe es una tarea común en la programación. Se trata de verificar si una ubicación en el sistema de archivos está presente o no. Los programadores a menudo necesitan hacer esto para asegurarse de que el programa pueda acceder a los archivos o para realizar acciones específicas dependiendo de si el directorio existe o no.

## Cómo:
La forma más fácil de comprobar si un directorio existe en Elm es utilizando la función `Directory.doesDirectoryExist`. Esta función toma una ruta (path) de un directorio como argumento y devuelve un `Task Bool` que indica si el directorio existe o no.

```
import Directory

main : Task Bool
main =
  Directory.doesDirectoryExist "ruta/del/directorio"
```

Si el directorio existe, el `Task` devolverá `Ok True`, si no existe, devolverá `Err False`. También se puede utilizar la función `Directory.getCurrentDirectory` para obtener la ruta actual del directorio y luego comprobar si existe utilizando `doesDirectoryExist`.

```
import Directory

main : Task Bool
main =
  Directory.getCurrentDirectory
    |> Task.andThen Directory.doesDirectoryExist
```

## Profundizando:
En el pasado, comprobar si un directorio existía requería una llamada al sistema operativo, lo que podía variar de una plataforma a otra. Con Elm, se utiliza una biblioteca externa llamada `elm/file` para manejar estas tareas de verificación. Otra alternativa es utilizar comandos nativos en Elm para llamar a funciones del sistema operativo.

## Ver también:
- [Elm Guide: File System](https://guide.elm-lang.org/io/files.html#directories)
- [elm/file](https://package.elm-lang.org/packages/elm/file/latest/) library