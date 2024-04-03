---
date: 2024-01-19
description: "How to: Elm har ikke direkte tilgang til stderr som ren funksjonell\
  \ spr\xE5k. Du kan utf\xF8re sideeffekter som loggf\xF8ring via ports sammen med\
  \ JavaScript."
lastmod: '2024-03-13T22:44:40.725097-06:00'
model: unknown
summary: "Elm har ikke direkte tilgang til stderr som ren funksjonell spr\xE5k."
title: Skrive til standardfeil
weight: 25
---

## How to:
Elm har ikke direkte tilgang til stderr som ren funksjonell språk. Du kan utføre sideeffekter som loggføring via ports sammen med JavaScript.

```Elm
port module Main exposing (..)

-- Definer en port for å sende feilmeldinger
port error : String -> Cmd msg

-- Bruk porten i en oppdateringsfunksjon
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ErrorOccurred errMsg ->
            ( model, error errMsg )

-- Eksempel på JavaScript-kode for å motta feil
app.ports.error.subscribe(function(errMsg) {
    console.error("Elm Error:", errMsg);
});
```

## Deep Dive
Direkte skriving til stderr er vanlig i imperativ programmering, men Elm isolerer sideeffekter. Ports gir en løsning hvor Elm og JavaScript kommuniserer. Alternativt kan du bruke `Debug.log` som logger til konsollen, men det er bare for utvikling, ikke produksjon.

## See Also
- Elm Ports: [guide.elm-lang.org/interop/ports.html](https://guide.elm-lang.org/interop/ports.html)
- JavaScript console.error: [developer.mozilla.org/en-US/docs/Web/API/Console/error](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)
