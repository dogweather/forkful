---
title:                "Ottenere la data corrente"
html_title:           "Elm: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché 
 Se ti piace programmare con un linguaggio funzionale e vuoi imparare qualcosa di nuovo, allora conoscere come ottenere la data corrente in Elm potrebbe essere un'ottima sfida per te.

## Come Fare
Per ottenere la data corrente in Elm, è necessario utilizzare il modulo `Time`. Questo modulo fornisce una funzione chiamata `now`, che restituisce un valore `Posix` rappresentante il timestamp corrente. Iniziamo importando il modulo `Time` e utilizzando la funzione `now`:

```Elm
import Time exposing (now)

main =
  let
    currentTime = now
  in
    text (toString currentTime)
```

L'output di questo codice sarà qualcosa del tipo `Posix 1551880822285`. Tuttavia, il risultato non è molto leggibile poiché è solo un valore numerico. Per renderlo più leggibile, è possibile utilizzare la funzione `millisToUtcDate`, che restituirà una stringa formattata con la data e l'ora correnti nella timezone UTC.

```Elm
import Time exposing (now)
import Time.Date exposing (millisToUtcDate)

main =
  let
    currentTime = now
    formattedTime = millisToUtcDate currentTime
  in
    text (toString formattedTime)
```

Adesso l'output dovrebbe essere qualcosa del tipo `DateTime 2019 Mar 6 12 58 49 219` che corrisponde alla data e all'ora attuali nella timezone UTC. Se desideri visualizzare la data e l'ora nella tua timezone locale, puoi utilizzare la funzione `millisToLocalDate` invece di `millisToUtcDate`.

## Deep Dive
La funzione `now` restituisce un valore `Task` perché è una chiamata asincrona. Ciò significa che dovremo gestire il valore `Task` utilizzando la funzione `Task.perform`, come mostrato nell'esempio seguente:

```Elm
import Time exposing (now)
import Platform.Cmd exposing (batch)
import Task exposing (Task, perform)
import Time.Date exposing (millisToUtcDate)

type Msg
  = GetTime (Result String (Time.Posix))

getTimeCmd : Task x (Time.Posix)
getTimeCmd =
  now

init : () -> ((), Cmd Msg)
init _ =
  ({}, perform GetTime (Ok >> millisToUtcDate) getTimeCmd)

update : Msg -> () -> ((), Cmd Msg)
update msg _ =
  case msg of
    GetTime result ->
      case result of
        Ok currentTime ->
          ({}, Cmd.none)
        Err error ->
          ({}, Cmd.none)

view : () -> Html Msg
view _ =
  text "Retrieving current time..."
```

Questa è solo una delle tante possibili implementazioni per gestire un valore `Task`. Puoi anche utilizzare la libreria di Timezone `elm-community/elm-time` per formattare la data e l'ora in modi diversi, oppure puoi creare una funzione personalizzata che restituisca solo la parte della data o dell'ora che ti interessa.

## Vedi Anche
- Documentazione ufficiale sul modulo `Time`: https://package.elm-lang.org/packages/elm/time/latest/Time
- Libreria `elm-community/elm-time`: https://package.elm-lang.org/packages/elm-community/elm-time/latest/