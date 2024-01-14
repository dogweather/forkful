---
title:    "Elm: Ottenere la data corrente"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Perché

Ottenere la data corrente è una funzionalità molto comune in programmazione e può essere utile in molte situazioni diverse. Ad esempio, è utile per mostrare la data in una pagina web, per generare report giornalieri o per calcolare l'età di una persona.

## Come fare

Per ottenere la data corrente in Elm, possiamo utilizzare la funzione predefinita `Time.now`. Questa funzione restituisce la data e l'ora attuali come un record contenente le informazioni sul fuso orario e l'offset rispetto all'ora di Greenwich.

````Elm
import Time exposing (now)

main = 
  now
  |> Debug.log "Data corrente"
````

Output:

````
Data corrente: { millis = 1607379235123, posix = Time.Posix 1607379235 123000, zone = Time.Zone.utc, offset = 0 }
````

Possiamo anche ottenere la data corrente con una formattazione specifica utilizzando la funzione `Time.format`. Ad esempio, se vogliamo ottenere solo il giorno, il mese e l'anno nella forma "gg/mm/aaaa", possiamo fare così:

````Elm
import Time exposing (now, format, Date)

main =
  now
  |> format "%d/%m/%Y"
  |> Debug.log "Data corrente formattata"
````

Output:

````
Data corrente formattata: 08/12/2020
````

## Approfondimento

Per ottenere una maggiore precisione nella data, possiamo utilizzare la libreria `elm/time` che ci permette di lavorare con date, tempi e fusi orari in maniera più dettagliata.

Inoltre, possiamo utilizzare la funzione `Time.fromIso` per convertire una stringa ISO 8601 in un oggetto `Time`. Questo può essere utile se vogliamo ottenere la data da una fonte esterna come un database o un API.

## Vedi anche

- [Documentazione ufficiale di Elm sul modulo Time](https://package.elm-lang.org/packages/elm/time/latest/)
- [ISO 8601 su Wikipedia](https://it.wikipedia.org/wiki/ISO_8601)
- [Libreria Moment.js per gestire date e fusi orari in JavaScript](https://momentjs.com/)