---
title:    "Elm: Comprobando si existe un directorio"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Por qué verificar la existencia de un directorio?

La verificación de la existencia de un directorio es importante para garantizar que nuestro programa pueda acceder a los archivos y directorios específicos que necesita. Esto nos permite asegurarnos de que nuestro código funcione correctamente y evitar errores no deseados. En esta publicación, aprenderemos cómo verificar si un directorio existe en Elm y cómo podemos manejar diferentes situaciones.

## Cómo hacerlo

```Elm
-- Importamos la biblioteca `File` que contiene funciones para trabajar con archivos y directorios
import File

-- Definimos una función que toma como parámetro una ruta de directorio
existeDirectorio : String -> Cmd msg
existeDirectorio ruta =
    -- Utilizamos la función `File.exists` para verificar si el directorio existe
    File.exists ruta
        |> Task.attempt DirectorioExistente

-- Creamos un comando para manejar el resultado de nuestra función
type Msg
  = DirectorioExistente Bool

-- Utilizamos la función `view` para mostrar el resultado en la página
view : Bool -> Html Msg
view existe =
    if existe then
        text "¡El directorio existe!"
    else
        text "Lo siento, el directorio no existe."

-- Un ejemplo de cómo usar nuestra función
main : Program () () Msg
main =
    Html.program
        { init = (existeDirectorio "miDirectorio", Cmd.none)
        , update = \msg model -> (model, Cmd.none)
        , subscriptions = always Sub.none
        , view = view
        }
```
**Ejemplo de salida:**

```
¡El directorio existe! 
```
```
Lo siento, el directorio no existe.
```

## Profundizar

La función `File.exists` que utilizamos en nuestro ejemplo devuelve una tarea que proporciona un valor booleano que indica si el directorio existe o no. Sin embargo, esta función solo verifica si el directorio existe en el momento en que se llama. Si el directorio se elimina o se mueve después de que se haya llamado a `File.exists`, nuestra función puede no reflejar la realidad.

Para verificar si el directorio existe en tiempo real, podemos utilizar la biblioteca `elm-lang/navigation` para suscribirnos a cambios en la URL y actualizar nuestro estado en consecuencia.

## Ver también

- Documentación de Elm: https://guide.elm-lang.org/architecture/user_input/buttons.html
- Elm Package: https://package.elm-lang.org/packages/elm-lang/navigation/latest/
- Ejemplos de código: https://github.com/elm-lang/examples/tree/master/navigation