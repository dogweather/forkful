---
title:    "Elm: Leyendo argumentos de línea de comandos"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por qué

Muchas veces, al programar en Elm, es necesario recibir información externa para ejecutar el programa de manera diferente. Una forma de hacer esto es a través de los argumentos de línea de comandos. En esta publicación, te explicaremos por qué es importante saber cómo leer estos argumentos y cómo puedes hacerlo de manera sencilla.

## Cómo hacerlo

¡Es muy simple leer argumentos desde la línea de comandos en Elm! Primero, importa el módulo `Platform` y utiliza la función `programWithFlags` para recibir los argumentos. Luego, puedes acceder a los argumentos a través de la función `flags` en `Html.program`.

```Elm
import Platform

-- código restante aquí

main =
  Platform.programWithFlags
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
    
-- más código aquí

view model =
  text (String.join " " (Platform.program.flags model))
```

Si ejecutas este código y le pasas argumentos desde la línea de comandos, se mostrarán en la vista.

## Profundizando

¿Quieres un mayor control sobre cómo leer y utilizar los argumentos de línea de comandos en Elm? Puedes hacer eso a través de la función `programWithApp` en lugar de `programWithFlags`. Esto te permitirá definir una función personalizada para manejar los argumentos y enviarlos a la función `view`.

Por supuesto, también puedes leer argumentos de línea de comandos en bibliotecas Elm, no solo en aplicaciones completas. Esto puede ser útil para personalizar comportamientos en distintos entornos de manera dinámica.

## Ver también

- Documentación oficial de Elm sobre [argumentos de línea de comandos](https://elm-lang.org/news/farewell-to-flags)
- Ejemplo de código utilizando `Platform.programWithFlags` para [leer argumentos](https://github.com/elm/compiler/blob/0.19.0/hot-mess/HotMess.elm)
- Blog [¿Por qué es importante leer argumentos de línea de comandos en Elm?](https://www.elmdigest.com/posts/command-line-arguments)