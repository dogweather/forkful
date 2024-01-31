---
title:                "Estrarre una data da una stringa"
date:                  2024-01-20T15:35:55.779124-07:00
html_title:           "Arduino: Estrarre una data da una stringa"
simple_title:         "Estrarre una data da una stringa"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Tradurre una data da una stringa significa convertirla in una struttura che il computer può comprendere e manipolare. Programmatori lo fanno per facilitare l'input dell'utente, il salvataggio di dati e per le operazioni di confronto e calcolo sulle date.

## How to:
Elm non ha una libreria standard per fare il parsing delle date da stringhe, quindi bisogna affidarsi a pacchetti di terze parti, come `justinmimbs/date`.

```Elm
import Date
import Date.Format exposing (iso8601)

parseDate : String -> Result String Date.Date
parseDate str =
    case Date.fromString str of
        Ok date -> Ok date
        Err error -> Err "Data non valida"

-- Esempio d'uso
result = parseDate "2021-03-19T00:00:00Z"
-- Risultato: Ok { year = 2021, month = 3, day = 19, hour = 0, minute = 0, second = 0, millisecond = 0, zone = Z }
```

Ricorda di gestire l'errore nel caso la stringa non sia una data valida.

## Deep Dive
In passato Elm aveva un modulo `Date` nativo, ma è stato deprecato in favore di soluzioni di terze parti più robuste e con maggiori funzionalità. Utilizzando pacchetti come `justinmimbs/date`, si ottengono maggiori benefici, come il supporto per timezone e formati personalizzati. Il parsing di date da stringhe richiede attenzione, soprattutto per le diverse rappresentazioni (es. americane vs europee). Assicurati che il formato sia chiaro e consistente nel tuo applicativo per evitare confusione.

## See Also
- Elm Date documentation: https://package.elm-lang.org/packages/elm/time/latest/
- Date parsing package `justinmimbs/date`: https://package.elm-lang.org/packages/justinmimbs/date/latest/
- Forum Elm: dove si può discutere e chiedere aiuto - https://discourse.elm-lang.org/
- Elm ISO8601 Date strings: dettagli e esempi sul formato data - https://en.wikipedia.org/wiki/ISO_8601
