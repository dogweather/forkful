---
title:    "Elm: Creando un archivo temporal"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Por qué crear un archivo temporal en Elm

Crear archivos temporales puede ser una herramienta útil en la programación de Elm. Ya sea para trabajar con datos sensibles o para realizar una tarea específica, los archivos temporales pueden ser una solución efectiva. A continuación, te explicaremos cómo crear un archivo temporal en Elm.

## Cómo crear un archivo temporal en Elm

Crear un archivo temporal en Elm es bastante simple. Todo lo que necesitas es importar el módulo `File` y utilizar la función `tempFile`.

```Elm
import File exposing (tempFile)

tempFile : String -> Cmd msg
```

La función `tempFile` toma como argumento una cadena de texto que será el nombre del archivo temporal a crear. También puedes proporcionar un segundo argumento opcional que especifica la ubicación del archivo. Si no se proporciona este argumento, el archivo se creará en la misma ubicación donde se ejecuta la aplicación Elm.

Ahora que sabes cómo llamar a la función `tempFile`, aquí hay un ejemplo de cómo usarla:

```Elm
import File exposing (tempFile)
import Html

main =
    Html.div []
        [ Html.text "Creando archivo temporal..."
        , tempFile "datos_sensibles.txt"
        ]
```

El código anterior creará un archivo temporal llamado "datos_sensibles.txt" en la misma ubicación donde se ejecuta la aplicación Elm. Si deseas especificar una ubicación diferente, puedes usar el segundo argumento de la función `tempFile` como se mencionó anteriormente.

## Profundizando en la creación de archivos temporales

Para aquellos que quieran conocer más, aquí hay algunos detalles sobre cómo se crean los archivos temporales en Elm. La función `tempFile` utiliza una combinación de números aleatorios y la hora actual para generar un nombre único para el archivo temporal. Además, también se encarga de eliminar el archivo una vez que se cierra la aplicación o cuando se elimina manualmente mediante la función `File.delete` del módulo `File`.

Una cosa importante a tener en cuenta es que los archivos temporales no son persistentes y se eliminan cada vez que se cierra la aplicación. Si necesitas que los datos contenidos en el archivo persistan, deberás guardarlos en una ubicación diferente.

# Ver también
- [Documentación de Elm sobre creación de archivos temporales](https://package.elm-lang.org/packages/elm/file/latest/File#tempFile)
- [Tutorial de Elm: Trabajar con archivos](https://www.elm-tutorial.org/en/08-creating-projects/02-working-with-files.html)
- [Ejemplo de creación de archivos temporales en Elm](https://gist.github.com/tusharmath/0b482601f8433d06ff2a7b4c4674f0a6)