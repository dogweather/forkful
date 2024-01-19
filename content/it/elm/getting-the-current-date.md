---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Ottenere la data corrente consente ai programmatori di raccogliere e salvare informazioni su quando un evento è accaduto nel sistema. Questa funzione è spesso utilizzata per registrare gli accessi o monitorare le operazioni degli utenti.

## Come fare:

Ecco un esempio di come ottenere la data corrente in Elm:

```Elm
import Time exposing (..)

main =
  Time.now
  |> Task.perform identity Debug.toString
  |> Html.program String
```

Questo codice restituisce un timestamp Unix come una stringa.

## Approfondimenti

La necessità di monitorare il tempo e la data ha origini storiche e risale alla nascita dell'informatica. In Elm, usiamo il modulo Time incluso nel linguaggio. Esistono altre librerie come `elm-date-extra` se avete bisogno di funzionalità extra.

In termini di implementazione, `Time.now` restituisce un `Task` piuttosto che un semplice `Time.Posix` a causa del suo comportamento asincrono. Questo significa che ottenere l'ora attuale può avere un piccolo ritardo, ma in pratica questo non genera problemi all'utente.

## Vedi anche

Per maggiori dettagli:

1. Documentazione di Elm's Time: http://package.elm-lang.org/packages/elm/time/latest
2. Forum di Elm: https://discourse.elm-lang.org/
3. Un pacchetto utile: http://package.elm-lang.org/packages/elm-community/elm-date-extra/latest