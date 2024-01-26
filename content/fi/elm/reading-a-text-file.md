---
title:                "Tekstitiedoston lukeminen"
date:                  2024-01-20T17:54:31.428466-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Tekstitiedoston lukeminen tarkoittaa tiedoston sisältämän tekstimateriaalin tuomista ohjelman käyttöön. Ohjelmoijat tekevät tämän, koska tiedostojen kautta voidaan käsitellä ja hyödyntää ulkoista dataa.

## How to:
Elmissä tiedoston lukeminen tapahtuu yleensä JavaScriptin interaktiivisen toiminnan, kuten `FileReader` API:n kautta, käyttäen portaaleja (Elm ports). Tässä esimerkki:

```Elm
port module Main exposing (..)

-- Määritä portti tiedoston sisällön vastaanottamiseen
port fileContent : (String -> msg) -> Sub msg

type Msg
    = ReceiveContent String

-- Aloita tiedoston luku kun portti vastaanottaa datan
subscriptions : Model -> Sub Msg
subscriptions model =
    fileContent ReceiveContent

-- Käsittele vastaanotettu tiedoston sisältö
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReceiveContent content ->
            ({ model | fileData = Just content }, Cmd.none)

-- Näytä tiedoston sisältö käyttöliittymässä
view : Model -> Html Msg
view model =
    div []
        [ case model.fileData of
              Just content ->
                  text content
              Nothing ->
                  text "Ei vielä luettua sisältöä." ]
```

JavaScriptissä voitaisiin käyttää jotain tällaista synkronoidaksemme Elm-koodin kanssa:

```JavaScript
// Liitä HTML-elementissä tapahtuva tiedoston valinta
document.getElementById('file-input').addEventListener('change', function(event) {
    var reader = new FileReader();
    reader.onload = function(event) {
        var contents = event.target.result;
        app.ports.fileContent.send(contents);
    };
    reader.readAsText(event.target.files[0]);
});
```

## Deep Dive
Elmissä ei ole sisäänrakennettua tapaa tiedostojen lukemiselle, koska se keskittyy puhtaiden funktionaalisten paradigmojen noudattamiseen ja sivuvaikutusten minimoimiseen. Historiallisesti tiedostojen käsittely on tapahtunut JavaScript-rajapinnan kautta, mitä kutsutaan portaaleiksi (ports). Vaihtoehtoisesti tiedoston lukemisen voisi hoitaa myös palvelimella, josta data siirretään HTTP:n avulla.

Vaikka Elm tarjoaa hyvän perustan frontend-sovelluksille, tiedoston lukemista on käsiteltävä JS:n maailmassa, koska se on selaimelle ominaista toiminnallisuutta. Se tarkoittaa sitä, että tietyt toiminnallisuudet ovat sovelluskehyksen ulkopuolella.

## See Also
- Elm Portaaleista: https://guide.elm-lang.org/interop/ports.html
- FileReader API: https://developer.mozilla.org/en-US/docs/Web/API/FileReader
- Elm ja HTTP: https://package.elm-lang.org/packages/elm/http/latest/
