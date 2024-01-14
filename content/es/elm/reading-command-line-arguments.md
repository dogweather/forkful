---
title:    "Elm: Leyendo argumentos de línea de comando"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué
Muchas veces, cuando estamos escribiendo código en Elm, nos encontramos con la necesidad de leer argumentos de línea de comandos. Esta tarea puede parecer abrumadora al principio, pero conocer cómo hacerlo nos permite agregar un nivel de interactividad a nuestras aplicaciones.

## Cómo hacerlo
La lectura de argumentos de línea de comandos en Elm se puede lograr utilizando la función `Platform.Cmd.parse`, que toma un `String` y devuelve una lista de argumentos en forma de `List String`. Veamos un ejemplo de cómo usarlo:

```Elm
import Platform.Cmd exposing (parse)

main : Platform.Cmd.Cmd msg
main =
  let
    args =
      Platform.Cmd.parse "--port 8080 --debug"
  in
    case args of
      Ok arguments ->
        Html.text ("Puerto: " ++ List.head arguments ++ "\nModo debug: " ++ List.head (List.tail arguments))

      Err error ->
        Html.text "¡Algo salió mal!"
```
El código anterior muestra cómo podemos utilizar la función `Platform.Cmd.parse` para leer argumentos de línea de comandos, en este caso, estamos leyendo el puerto y el modo de depuración. La salida de este programa será:

```
Puerto: 8080
Modo debug: --debug
```

## Profundizando
Ahora que sabemos cómo leer argumentos de línea de comandos en Elm, podemos profundizar un poco más en el tema. Además de la función `parse`, también podemos utilizar la función `Platform.Cmd.args` que nos devuelve una lista de argumentos, incluyendo el nombre del comando en sí. Además, también podemos utilizar la función `Platform.Cmd.flag` para leer un argumento específico en lugar de una lista completa.

## Ver también
- [Documentación de Elm sobre línea de comandos](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd)
- [Ejemplos de código en Elm sobre línea de comandos](https://github.com/elm/project-scaffolding/tree/master/command-line-app)