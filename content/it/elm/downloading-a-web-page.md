---
title:                "Scaricare una pagina web"
html_title:           "C++: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Scaricare una pagina web significa ottenere una copia locale del contenuto di un sito web. I programmatori lo fanno per analizzare il contenuto dei siti web, eseguire test di funzionalità, e per raccogliere dati.

## Come fare

Purtroppo Elm, come molti altri linguaggi puramente funzionali, non supporta nativamente il download delle pagine web. Tuttavia, quando Elm è usato in combinazione con JavaScript, è possibile effettuare tali richieste. Considera l'esempio seguente:

```Elm
port module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

port taskRunner : (Int -> Cmd msg) -> Sub msg

main =
  Html.beginnerProgram { model = 0, view = view, update = update }
  
view model =
  div []
    [ button [ onClick Increment ] [ text "Increase" ]
    , div [] [ text (toString model) ]
    ]

type Msg = Increment

update msg model =
  case msg of
    Increment ->
      { model = model + 1
      , cmd = taskRunner model
      }

port increment : Int -> Cmd msg 
```

Questo pezzo di codice Elm invoca una funzione JavaScript esterna ogni volta che l'utente fa clic sul pulsante. Ora, il download della pagina web lo eseguirai con JavaScript.

```JavaScript
var app = Elm.Main.fullscreen();
app.ports.taskRunner.subscribe(function(url) {
  fetch(url)
    .then(response => response.text())
    .then(data => console.log(data))
    .catch(error => console.error('Error:', error));
});
```

## Analisi Approfondita

Elm è stato creato da Evan Czaplicki nel 2012 come front-end per le applicazioni web. Poiché Elm è un linguaggio puramente funzionale, non consente operazioni di I/O come il download di pagine web, e quindi tali attività devono essere gestite da JavaScript.

Esistono alternative ad Elm, tra cui JavaScript puro, TypeScript, e ClojureScript che hanno funzionalità di I/O incorporate.

L'implemetazione specifica del download delle pagine web in Elm dipende da come viene utilizzato il linguaggio in combinazione con JavaScript, come visto nella sezione di "Come fare".

## Vedi Anche 

Per continuare a imparare Elm, fai riferimento alle seguenti risorse:

- [Guida ufficiale di Elm](https://guide.elm-lang.org)
- [Elm Cheat Sheet](https://github.com/izdi/elm-cheat-sheet)
  
Per approfondire gli argomenti relativi al download delle pagine web, considera le seguenti risorse:

- [Fetch API](https://developer.mozilla.org/it/docs/Web/API/Fetch_API)
- [JavaScript Promises: an Introduction](https://developers.google.com/web/fundamentals/primers/promises)