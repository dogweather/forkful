---
title:                "Elm: Creación de un archivo temporal"
simple_title:         "Creación de un archivo temporal"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué
En la programación, a veces es necesario crear archivos temporales para almacenar datos de forma eficiente o para realizar ciertas operaciones. En Elm, también hay una forma de hacerlo y en este artículo te voy a enseñar cómo.

## Cómo hacerlo
Para crear un archivo temporal en Elm, podemos utilizar la función "File.temp" que se encuentra en el módulo "File". Veamos un ejemplo de cómo crear un archivo temporal y escribir un texto en él:

``` Elm
import File

main =
  File.temp "blog.txt"
    |> Result.map .path
    |> Result.andThen (File.write "¡Hola mundo!")
```

En este ejemplo, primero importamos el módulo "File" y luego llamamos a la función "File.temp" con el nombre que queremos darle a nuestro archivo temporal. Luego, utilizamos la función "Result.map" para obtener la ruta del archivo y la función "Result.andThen" para escribir el texto "¡Hola mundo!" dentro de él.

## Deep Dive
Ahora, profundicemos un poco más en la creación de archivos temporales en Elm. La función "File.temp" también nos permite especificar la ubicación y el tipo de archivo que queremos crear. Por ejemplo, podemos crear un archivo temporal en una carpeta específica y especificar que sea un archivo de texto.

Otra cosa interesante que podemos hacer es utilizar el módulo "System.File" para obtener información sobre el archivo temporal que acabamos de crear, como su tamaño o la fecha en la que se creó. Esto puede ser útil si queremos realizar operaciones adicionales con el archivo temporal.

## Ver también
- [Documentación oficial de Elm sobre la función "File.temp"](https://package.elm-lang.org/packages/elm/bytes/latest/File#temp)
- [Módulo System.File](https://package.elm-lang.org/packages/elm/core/latest/System-File)