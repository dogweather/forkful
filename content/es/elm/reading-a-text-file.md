---
title:    "Elm: Leyendo un archivo de texto."
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por qué 

Existen muchas razones por las cuales es importante leer un archivo de texto al programar en Elm. Una de ellas es que te permite acceder y manipular grandes cantidades de datos almacenados en un mismo archivo de forma eficiente y estructurada. También es una herramienta útil para el manejo de archivos en la web, como por ejemplo en la carga de archivos de configuración o de datos para una aplicación.

## Cómo 

Para leer un archivo de texto en Elm, se utiliza la función `Text.fromFile`, la cual toma como parámetro la ruta del archivo y devuelve un objeto `Task Text` que representa una "tarea" a ser realizada en un futuro. Esta tarea se puede encadenar con otras acciones utilizando la función `Task.andThen` y finalmente se puede ejecutar utilizando la función `Task.attempt`, que devuelve un `Task Never ()` que debe ser ejecutado con la función `Cmd.execute`.

A continuación se muestra un ejemplo de código que lee un archivo de texto y muestra su contenido en la consola:

```Elm
import Text exposing (Text)
import Task exposing (Task)
import Cmd exposing (Cmd)

main =
  Text.fromFile "path/to/file.txt"
    |> Task.andThen handleResult
    |> Task.attempt never

handleResult : Text -> Task Never ()
handleResult text =
  text
    |> Debug.log "Texto leido:"
    |> Cmd.log

never : Never
never =
  Debug.crash "Tarea nunca debería ser ejecutada"
```

El resultado de este código será imprimir en la consola el contenido del archivo de texto leído.

## Profundizando 

La función `Text.fromFile` es capaz de manejar archivos con diferentes tipos de codificación, como UTF-8, UTF-16 o ISO-8859-1, utilizando la opción `Text.Encoding` que se le puede pasar como un segundo parámetro. También es posible especificar un tamaño máximo de lectura de caracteres en la opción `Text.limit`, lo cual es útil en caso de archivos muy grandes para evitar errores de memoria.

Además, es importante tener en cuenta que la función `Text.fromFile` devuelve un objeto `Task Text`, el cual puede representar tanto un éxito como un fallo. Por lo tanto, es necesario utilizar la función `Task.attempt`, que maneja ambos casos y devuelve un objeto `Task Never ()` en caso de éxito o un `Task Http.Error ()` en caso de fallo.

## Ver también 

- [Documentación oficial de la función `Text.fromFile` en Elm](https://package.elm-lang.org/packages/elm/core/latest/Text#fromFile)
- [Tutorial sobre manejo de archivos en Elm](https://elmprogramming.com/file-reading-and-writing.html)
- [Ejemplo de lectura de archivos en Elm en conjunto con el framework Phoenix](https://medium.com/@drupalmujica/reading-files-in-elm-from-your-phoenix-framework-546870980a5b)