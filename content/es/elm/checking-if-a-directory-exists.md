---
title:                "Elm: Comprobando si existe un directorio"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

##Por qué 

Revisar si un directorio existe puede ser una tarea importante en ciertas aplicaciones de Elm. Es útil para asegurarse de que un directorio en particular esté disponible antes de realizar alguna acción, como guardar archivos o leer su contenido. Además, puede ayudar a manejar errores de manera más efectiva.

##Cómo hacerlo

Para revisar si un directorio existe en Elm, primero debemos importar el módulo `Directory` de la biblioteca `elm/file`. Luego, podemos utilizar la función `exists` que toma un `path` (ruta) y devuelve un `Cmd` (comando) que retorna un resultado `Bool` indicando si el directorio existe o no.

```Elm
import File.Directory exposing (exists)

checkDirectory : Cmd msg
checkDirectory = exists "ruta/a/mi/directorio"
```

La variable `checkDirectory` ahora contiene un comando que podemos ejecutar utilizando `Platform.send` o `Task.perform`. Podemos utilizar estas funciones dentro de un `update` o en un `Cmd` independiente para obtener el resultado de si el directorio existe o no.

```Elm
import File.Directory exposing (exists)

type Msg
    = DirectorioExiste Bool

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        DirectorioExiste existe ->
            if existe then { model | mensaje = "El directorio existe." } else { model | mensaje = "El directorio no existe." } ! []

main : Program Never Model Msg 
main = 
    Browser.sandbox
        { init = { mensaje = "" }
        , view = view
        , update = update
        }

view : Model -> Html Msg
view model = 
    div []
        [ h1 [] [ text "Verificar si un directorio existe en Elm." ]
        , p [] [ text model.mensaje ]
        , button [ onClick checkDirectory ] [ text "¡Haz clic para comprobar el directorio!" ]
        ]
```

Esta es solo una forma simple de implementar la verificación de un directorio existente en Elm. El comando `checkDirectory` se ejecutará al hacer clic en el botón y se actualizará el modelo con el resultado en el campo `mensaje`.

##Deep Dive

En la mayoría de los casos, la función `exists` solo verificará si el directorio existe, pero no si es un directorio vacío o no. Sin embargo, en algunas ocasiones, podemos querer asegurarnos de que el directorio no solo exista, sino que también tenga algún contenido. En este caso, podemos utilizar la función `listFiles` para obtener una lista de los archivos en el directorio y verificar su tamaño. Si la lista está vacía, sabremos que el directorio existe pero está vacío.

```Elm
import File.Directory exposing (exists, listFiles) 
import File.Selectors exposing (fileSize) 

checkDirectory : Cmd msg 
checkDirectory = 
    exists "ruta/a/mi/directorio" 
        |> andThen 
            ( \exists -> 
                if exists then 
                    listFiles "ruta/a/mi/directorio" 
                        |> andThen ignore
                          -- ignore es una función de ayuda que simplemente descarta el resultado ya que solo nos interesa el tamaño.
                        |> andThen fileSize 
                        |> andThen 
                            ( \size -> 
                                if size == -1 then 
                                    Task.succeed False
                                else 
                                    Task.succeed True 
                            ) 
                else 
                    Task.succeed False
            )

```

##Vea también

- Documentación de la biblioteca `elm/file`: <https://package.elm-lang.org/packages/elm/file/latest/>
- Ejemplo de código en GitHub: <https://github.com/lumenghe/elm-file-directory>