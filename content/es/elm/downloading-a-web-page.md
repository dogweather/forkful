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

## Por qué

Descargar una página web es una tarea común para aquellos que trabajan con desarrollo web. Ya sea para realizar pruebas, analizar el código fuente o simplemente guardar una copia local, la capacidad de descargar una página web puede resultar muy útil. En este artículo, aprenderemos a hacerlo utilizando Elm.

## Cómo hacerlo

La forma más sencilla de descargar una página web en Elm es utilizando la función `Http.get` del módulo `Http`. Esta función toma dos parámetros: una URL y una función de manejo de errores. Por ejemplo, si queremos descargar la página de Google, podríamos escribir lo siguiente:

```
Elm
import Http

Http.get "https://google.com" handleResponse
```

Luego, necesitamos definir la función `handleResponse` que manejará la respuesta de la petición. Esta función deberá tener un parámetro que represente la respuesta de la petición y devolver un modelo de datos que indique cómo manejar esa información. Por ejemplo, si queremos mostrar el código fuente de la página descargada en un `div`, podríamos escribir lo siguiente:

```
Elm
import Html exposing (div, text)
import Http

handleResponse : Http.Response -> Html.Html
handleResponse response =
    div [] [ text response.body ]

Http.get "https://google.com" handleResponse
```

Al ejecutar este código, veremos que se muestra en la vista previa el código fuente de la página de Google.

## Profundizando

La función `Http.get` también puede tomar un tercer parámetro opcional, que es un diccionario de encabezados. Esta opción nos permite especificar encabezados adicionales para la petición, como por ejemplo, si necesitamos autenticación para acceder a la página solicitada. También podemos utilizar la función `Http.getWith` que nos permite especificar el formato de la respuesta esperada, como por ejemplo `Http.text` para obtener el texto plano de la página, o `Http.blob` para obtener la página en formato binario.

## Ver también

- Documentación oficial de la función `Http.get` en la guía de Elm: https://guide.elm-lang.org/effects/http.html
- Ejemplo completo de descarga de una página web utilizando Elm: https://github.com/elm/compiler/issues/1160#issuecomment-122190364
- Otra forma de descargar una página web utilizando la librería `elm-lang/http`: https://package.elm-lang.org/packages/elm-lang/http/latest/Http#download