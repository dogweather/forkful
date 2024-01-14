---
title:                "Elm: Descargar una página web"
simple_title:         "Descargar una página web"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por qué

Los programadores necesitan descargar una página web por diversas razones, como extraer datos para análisis o automatizar procesos. Aprender a hacerlo en Elm puede ser una habilidad útil y emocionante.

## Cómo

Para descargar una página web en Elm, primero necesitas importar el módulo `Http` y `String`. Luego, puedes hacer una solicitud HTTP GET a la URL que deseas descargar. Aquí hay un ejemplo de código que descarga la página de inicio de Google y muestra el contenido en la consola:

```
Elm Http
String

main =
    Http.get "https://www.google.com"
        |> Task.perform (Debug.log "Contenido de la página" << String.fromCharList << Http.toString)
```

La salida de este código será una cadena de texto con todo el contenido de la página descargada. Dependiendo de tus necesidades, también puedes realizar otras solicitudes HTTP como POST o enviar encabezados personalizados.

## Buceo Profundo

Existen muchas herramientas y técnicas avanzadas para descargar páginas web en Elm. Por ejemplo, puedes utilizar la biblioteca `elm-lang/robots-txt` para validar si tienes permiso para descargar una determinada URL según las reglas del archivo robots.txt. También puedes utilizar decodificadores personalizados para extraer datos estructurados de la página descargada.

## Ver También

- [Documentación oficial de Elm sobre el módulo Http](https://elm-lang.org/docs/http)
- [Repositorio de elm-lang/robots-txt](https://github.com/elm-lang/robots-txt)
- [Tutorial de decodificadores personalizados en Elm](https://dev.to/alexkorzhikov/building-custom-decoders-in-elm-483h)