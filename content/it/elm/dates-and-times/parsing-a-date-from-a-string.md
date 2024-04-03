---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:21.886881-07:00
description: "Analizzare una data da una stringa in Elm comporta la trasformazione\
  \ delle informazioni testuali che rappresentano date e orari in un formato che Elm\
  \ pu\xF2\u2026"
lastmod: '2024-03-13T22:44:43.359038-06:00'
model: gpt-4-0125-preview
summary: "Analizzare una data da una stringa in Elm comporta la trasformazione delle\
  \ informazioni testuali che rappresentano date e orari in un formato che Elm pu\xF2\
  \ capire e manipolare, specificamente nel tipo `Date`."
title: Analisi di una data da una stringa
weight: 30
---

## Come fare:
Elm non dispone di capacità incorporate robuste come alcuni altri linguaggi per l'analisi delle date, facendo affidamento principalmente sull'interoperabilità con Javascript o su librerie per operazioni più complesse. Tuttavia, puoi utilizzare il pacchetto `elm/time` per un'analisi di base, e per esigenze più complesse, la libreria di terze parti `justinmimbs/date` è ampiamente raccomandata.

### Analisi utilizzando `elm/time`:
`elm/time` fornisce il modulo `Time`, che ti consente di lavorare con timestamp invece di date leggibili dall'uomo. Sebbene non analizzi direttamente le date dalle stringhe, puoi convertire una stringa ISO 8601 in un timestamp POSIX, con il quale puoi poi lavorare.

```elm
import Time exposing (Posix)

-- Assumendo che tu abbia una stringa di data ISO 8601
isoDateStr : String
isoDateStr = "2023-01-01T00:00:00Z"

-- Convertila in un timestamp POSIX (questa funzione restituisce un `Result`)
parsedDate : Result String Posix
parsedDate = Time.fromIsoString8601 isoDateStr

-- Output di esempio: Ok <valore tempo posix>
```

### Analisi utilizzando `justinmimbs/date`:
Per un'analisi più intricata, come il trattamento di formati non ISO, la libreria `justinmimbs/date` è un'ottima scelta. Ecco come puoi utilizzarla per analizzare una stringa di data personalizzata:

1. Assicurati di avere installato la libreria:

```shell
elm install justinmimbs/date
```

2. Utilizza la funzione `Date.fromString` per analizzare formati di date personalizzati:

```elm
import Date
import Result exposing (Result(..))

-- Diciamo che hai un formato di stringa di data personalizzata `dd-MM-yyyy`
customDateStr : String
customDateStr = "01-01-2023"

-- Funzione per analizzare il formato personalizzato
parseDate : String -> Result String Date.Date
parseDate = Date.fromString "dd-MM-yyyy"

-- Esempio di utilizzo
parsedCustomDate : Result String Date.Date
parsedCustomDate = parseDate customDateStr

-- Output di esempio: Ok (Date.fromCalendarDate 2023 Gen 1)
```

In questi esempi, il tipo `Result` incapsula o un'analisi riuscita che produce una data (`Ok`) o un errore (`Err`), permettendo una gestione degli errori robusta nelle tue applicazioni Elm.
