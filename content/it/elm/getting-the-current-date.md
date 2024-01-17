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

## Cosa & Perché?

Ottenere la data corrente è un'operazione comune quando si lavora con Elm. Questa operazione permette ai programmatori di accedere alla data corrente e utilizzarla per scopi come la gestione del tempo e la creazione di un vero e proprio calendario con funzioni di ricerca e calcolo. 

## Come fare:

Ecco un esempio di come ottenere la data corrente in Elm:
```Elm
import Time exposing (now)
import Time.Extra exposing (fromPosix)

now
    |> fromPosix
    |> toString
```
Esempio di output: "2021-07-21 11:30:00.000 UTC"

È possibile personalizzare il formato della data utilizzando la funzione `format` dal modulo Time. Ad esempio, per ottenere solo la data in formato "giorno/mese/anno":
```Elm
import Time exposing (now)
import Time.Format exposing (format)
import Date.Extra exposing (shortDate)

now
    |> format shortDate
    |> toString
```
Esempio di output: "21/07/2021"

## Analisi approfondita:

Ottenere la data corrente non è nulla di nuovo per i programmatori, ma è comunque un'operazione importante in qualsiasi linguaggio di programmazione. In passato, come il linguaggio JavaScript, doveva essere gestita utilizzando un oggetto chiamato `Date` che era noto per essere molto difficile da comprendere e da utilizzare. Tuttavia, con Elm, ottenere la data corrente è diventato molto più semplice e intuitivo grazie ai moduli `Time` e `Date`.

Alternativamente, si può anche utilizzare la libreria `elm-community/time-extra` che offre funzionalità più avanzate per la gestione della data e del tempo.

Implementare la funzione per ottenere la data corrente in Elm è abbastanza semplice. Il modulo `Time` offre la funzione `now` che restituisce la data corrente come un valore `Posix` (una rappresentazione in millisecondi del momento in cui viene chiamata la funzione). Quindi, per ottenere la data in un formato leggibile è necessario utilizzare la funzione `toString` dal modulo `Time.Extra` che restituirà la data come una stringa.

## Vedi anche:

- Documentazione del modulo Time in Elm: https://package.elm-lang.org/packages/elm/time/latest
- Documentazione del modulo Date in Elm: https://package.elm-lang.org/packages/elm/date/latest
- Libreria `elm-community/time-extra`: https://package.elm-lang.org/packages/elm-community/time-extra/latest/