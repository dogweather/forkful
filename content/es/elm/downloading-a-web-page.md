---
title:                "Descargando una página web."
html_title:           "Elm: Descargando una página web."
simple_title:         "Descargando una página web."
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Descargar una página web significa obtener el código y los recursos de dicha página desde un servidor y mostrarlos en tu navegador. Los programadores lo hacen para acceder a información específica de una página, como texto, imágenes o datos, y utilizarla en su aplicación.

## Cómo hacerlo:
```
Elm.Http.get "https://www.paginaweb.com/"
    |> Http.send handleResponse
    
handleResponse response =
    case response of
        Ok body ->
            Html.text "Contenido de la página web: " ++ body
        Err _ ->
            Html.text "No se pudo descargar la página web."
```

El código utiliza la librería `Http` de Elm para enviar una solicitud GET al servidor de la página web y obtener su contenido. Luego, se utiliza la función `text` de la librería `Html` para mostrar el contenido en la página.

## Profundizando:
El proceso de descargar una página web es una técnica común utilizada en el desarrollo web para obtener información de otras páginas y utilizarla en tu propia aplicación. Alternativas a Elm para realizar esta acción incluyen JavaScript, PHP y Python. La librería `Http` de Elm utiliza la función `send` para enviar una solicitud con una función de manejo que especifica cómo manejar la respuesta del servidor.

## Ver también:
- Documentación de la librería `Http` de Elm: https://package.elm-lang.org/packages/elm/http/latest/
- Ejemplos de código para descargar una página web con diferentes lenguajes de programación: https://www.geeksforgeeks.org/downloading-a-webpage-locally-with-curl-in-php/