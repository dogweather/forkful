---
title:                "Skrive til standardfeil"
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Skriving til standard feil (stderr) lar deg rapportere feil uten å blande det med hovedutdata. Programmerere bruker det til å skille normal output fra feilmeldinger.

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