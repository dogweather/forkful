---
title:                "Creando un archivo de texto"
html_title:           "Elm: Creando un archivo de texto"
simple_title:         "Creando un archivo de texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Escribir un archivo de texto es simplemente crear un documento de texto en un ordenador. Los programadores a menudo tienen que hacer esto para guardar y organizar información importante relacionada con su código, como comentarios, instrucciones y datos.

## Cómo:
```Elm
Elm.file "nombre_archivo.txt" (Just "¡Hola, mundo!")
```
Este ejemplo utiliza la función `file` de Elm para crear un archivo de texto llamado "nombre_archivo.txt" con el contenido "¡Hola, mundo!". Puedes reemplazar el texto con cualquier información que desees incluir en el archivo.

La función `file` toma dos argumentos: el nombre del archivo y el contenido. El contenido debe estar envuelto en un `Just`, que es una estructura de datos de Elm que significa "algo está aquí". De esta manera, Elm asegura que estás consciente de que estás guardando algo en el archivo.

Elm también proporciona una función `appendFile` para agregar contenido adicional a un archivo existente. Aquí hay un ejemplo de cómo usarlo:
```Elm
Elm.appendFile "nombre_archivo.txt" "¡Adiós, mundo!"
```
Esto agregará "¡Adiós, mundo!" al final del archivo "nombre_archivo.txt".

## Deep Dive:
Escribir archivos de texto ha sido una tarea básica para los programadores desde los inicios de la informática. En algunas lenguas de programación, esto se hace utilizando comandos de sistema operativo. Sin embargo, en Elm, se hace a través de funciones de la biblioteca `File` integrada.

Si prefieres no utilizar la biblioteca `File`, también puedes escribir archivos de texto utilizando la función `Http.send` y enviando una solicitud HTTP al servidor en el que se encuentra tu archivo.

## Ver también:
- [Documentación de la función file de Elm](https://package.elm-lang.org/packages/elm/file/latest/File#file)
- [Documentación de la función appendFile de Elm](https://package.elm-lang.org/packages/elm/file/latest/File#appendFile)
- [Documentación de la función Http.send de Elm](https://package.elm-lang.org/packages/elm/http/latest/Http#send)