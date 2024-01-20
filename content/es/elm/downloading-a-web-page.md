---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Descargar una página web se refiere a obtener su código fuente y guardarla en el disco duro. Los programadores suelen hacerlo para analizar la estructura de la página, probar su funcionalidad offline, o para recopilar datos y contenido en su propio proyecto.

## Cómo se hace:

Usando Elm, el módulo `Http` permite hacer este tipo de tareas fácilmente. Aquí tienes un ejemplo simple:

```Elm
import Html exposing (Html, text)
import Http
import Json.Decode as Decode

descargaUrl : String
descargaUrl =
    "http://miweb.com"

fetchUrl : Cmd Msg
fetchUrl =
    Http.get { url = descargaUrl, expect = Http.expectString GotResponse }

type Msg
    = GotResponse (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse result ->
            case result of
                Err _ ->
                    ( model, fetchUrl )

                Ok respuesta ->
                    ( respuesta, Cmd.none )

view : Model -> Html Msg
view respuesta =
    text respuesta
```

Este código va a obtener la página web en "http://miweb.com". El resultado se almacenará en la variable `respuesta`.

## Un poco más profundo

Historia: La capacidad de descargar páginas web comenzó a tener importancia después del nacimiento del internet. Como las páginas web comenzaron a ser más interactivas y dinámicas, las técnicas para descargar estas evolucionaron.

Alternativas: En Elm, la forma más simple (como la mencionada anteriormente) es suficiente para muchos casos. Sin embargo, otras herramientas y lenguajes en otras plataformas pueden ofrecer funcionalidad más avanzada, como Python con su módulo `BeautifulSoup` o Node.js con `Puppeteer`.

Detalles de implementación: En Elm, las solicitudes HTTP son un poco diferentes ya que son inmutables y usan el modelo de arquitectura Elm (TEA). Todo se maneja a través de mensajes y comandos asíncronos, lo que nos lleva a tener código más seguro y predecible.

## Vea también.

- [Documentación oficial de Elm](https://guide.elm-lang.org/)
- [Documentación de Puppeteer en Node.js](https://developers.google.com/web/tools/puppeteer)