---
title:                "Leyendo argumentos de línea de comando."
html_title:           "Elm: Leyendo argumentos de línea de comando."
simple_title:         "Leyendo argumentos de línea de comando."
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo un programa de línea de comandos puede leer los argumentos que le pasas? Bueno, en Elm, es muy sencillo y hoy te voy a enseñar cómo hacerlo.

## Cómo hacerlo

Para leer los argumentos de línea de comando en Elm, utilizaremos la función `System.args` de la biblioteca `elm/core`. Esta función devuelve una lista de cadenas de texto que representan los argumentos pasados al programa. Veamos un ejemplo:

```Elm
import System exposing (..)

main =
  let
    args = System.args
  in
    case args of
      Ok arguments ->
         case arguments of
           [] -> -- Si no se pasa ningún argumento, se ejecuta este código
             "No se pasaron argumentos"
           (arg::_) -> -- Si se pasan argumentos, se ejecuta este código
             "El argumento es " ++ arg
      Err error -> -- En caso de error, se ejecuta este código
         "Hubo un error al leer los argumentos"
```

Veamos el posible resultado de este programa para diferentes argumentos pasados en la línea de comandos:

```
$ elm make my_program.elm
"No se pasaron argumentos"

$ elm make my_program.elm argumento
"El argumento es argumento"

$ elm make my_program.elm "otra cadena de texto"
"El argumento es otra cadena de texto"
```

## Profundizando

La función `System.args` también acepta un argumento opcional de tipo `Decoded.Value`, que se utiliza para decodificar los argumentos pasados en un formato específico. Por ejemplo, si pasamos argumentos en formato JSON, podemos utilizar la biblioteca `elm/json` para decodificarlos y utilizarlos en nuestro programa.

Además, podemos utilizar la función `System.execPath` para obtener la ruta de ejecución del programa, lo que puede ser útil en algunas situaciones.

## Ver también

Para obtener más información sobre cómo trabajar con programas de línea de comandos en Elm, puedes consultar la documentación oficial [aquí](https://package.elm-lang.org/packages/elm/core/latest/). También puedes explorar la biblioteca [elm-cli-options-parser](https://package.elm-lang.org/packages/MasterToad/elm-cli-options-parser/latest/) para una forma más avanzada de manejar los argumentos de línea de comando en Elm.