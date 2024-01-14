---
title:                "Elm: Creando un archivo temporal."
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

Muchos programadores usan archivos temporales en sus proyectos para almacenar datos temporales que no requieren ser guardados permanentemente en una base de datos. Algunos ejemplos comunes incluyen la generación de archivos CSV para exportar datos o la creación de imágenes temporales.

## Cómo hacerlo

Crear un archivo temporal en Elm es muy sencillo gracias al paquete `elm/file`. Aquí hay un ejemplo de cómo crear y escribir en un archivo temporal en Elm:

```elm
import File
import Task exposing (Task)

-- Función para crear un archivo temporal y escribir en él
createTempFile : String -> Task File.Error File
createTempFile content =
    File.temp "ejemplo.csv" content

-- Llamar a la función y ejecutar la tarea
tempFileTask : Task File.Error File
tempFileTask =
    createTempFile "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."

-- Manejar el resultado
case Task.perform identity identity tempFileTask of
    Ok file ->
        file

    Err err ->
        Debug.log "Error" err
```

Este código creará un archivo temporal llamado "ejemplo.csv" y escribirá el texto `"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."` en él.

Para leer el archivo temporal creado, se puede usar la función `File.read` y para eliminarlo, se puede usar `File.delete`.

## Profundizando

Crear un archivo temporal en Elm es relativamente simple, pero es importante tener en cuenta algunas cosas al trabajar con archivos temporales. Por ejemplo, siempre es una buena práctica asegurarse de que los archivos temporales sean eliminados después de su uso para no ocupar espacio innecesario en el sistema operativo.

También se puede utilizar la función `File.name` para obtener la ruta completa del archivo temporal creado y así facilitar su uso en otras tareas.

## Ver también

- [Documentación oficial de Elm sobre el paquete `elm/file`](https://package.elm-lang.org/packages/elm/file/latest/)
- [Tutorial de Creating an Elm App](https://ohanhi.github.io/base/index.html) con un ejemplo práctico de cómo crear y escribir en un archivo temporal en Elm.