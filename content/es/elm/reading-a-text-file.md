---
title:                "Elm: Leyendo un archivo de texto"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo leer un archivo de texto en Elm? Ya sea para procesar datos, leer una configuración o simplemente para experimentar con una nueva técnica de programación, leer un archivo de texto puede ser una habilidad muy útil. En esta publicación, exploraremos cómo leer un archivo de texto en Elm.

## Cómo hacerlo

Para leer un archivo de texto en Elm, primero debes importar el módulo `File` y `Platform` en tu archivo. Luego, puedes usar la función `File.toUrl` para convertir la ubicación del archivo en una URL y la función `Platform.worker` para crear un Worker que se encargará de realizar la lectura del archivo.

A continuación, en el código de Elm, puedes usar el comando `getFile` de `Platform.worker` para obtener el contenido del archivo como una `Task`. Luego, puedes usar la función `Task.perform` para indicar qué hacer con ese contenido. Por ejemplo, puedes imprimir el contenido en la consola o mostrarlo en la pantalla.

Aquí tienes un ejemplo de código que muestra cómo leer un archivo de texto en Elm y mostrar su contenido en la consola:

```Elm
import File
import Platform

main =
  let
    fileUrl = File.toUrl "myFile.txt"
    worker = Platform.worker (getFile fileUrl) |> Task.perform showContent
  in
    worker

getFile : File.Url -> Task File.Error String
getFile fileUrl =
  File.downloadString fileUrl

showContent : Result File.Error String -> Cmd msg
showContent result =
  case result of
    Ok content -> 
        Debug.log "Contenido del archivo:" content
        Cmd.none
    Err error -> 
        Debug.log "Error al leer el archivo:" error
        Cmd.none
```

## Profundiza en el tema

Además de simplemente leer el contenido de un archivo de texto, también puedes realizar otras acciones como buscar una cadena específica o escribir nuevos datos en el archivo. Para esto, puedes utilizar las diferentes funciones del módulo `File`, como `File.readLines`, `File.write`, entre otras.

También es importante tener en cuenta que al leer un archivo de texto en Elm, estás interactuando con el sistema de archivos del usuario, lo que puede presentar algunas limitaciones dependiendo de la plataforma en la que se esté ejecutando el código.

## Ver también

- Documentación oficial de Elm para leer y escribir en archivos de texto: https://elm-lang.org/docs/io
- Tutorial en español sobre cómo leer y escribir en archivos de texto en Elm: https://medium.com/@jorgegomar/how-to-read-and-write-files-in-elm-escribir-y-leer-archivos-en-elm-d502a9f4272a