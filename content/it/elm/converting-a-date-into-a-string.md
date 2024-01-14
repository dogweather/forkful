---
title:                "Elm: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Molti linguaggi di programmazione hanno diverse funzioni per manipolare e trasformare le date. E anche Elm offre una soluzione semplice per convertire una data in una stringa. Ma perché dovresti farlo? Ci sono diverse ragioni per le quali potresti voler convertire una data in una stringa. Ad esempio, potresti voler mostrare la data in un formato specifico per un'interfaccia utente o potresti voler salvare la data come stringa in un database.

## Come Fare

Per convertire una data in una stringa in Elm, puoi utilizzare la funzione `toString` del modulo `Date`. Ecco un esempio di come puoi utilizzarla:

```Elm
import Date exposing (..)

myDate : Date
myDate = Date.fromIsoString "2019-10-29T10:00:00Z"

myString : String
myString = Date.toString "%d/%m/%Y" myDate

```

Questo codice importerà il modulo `Date` e creerà una data utilizzando la funzione `fromIsoString` con il formato standard ISO: `YYYY-MM-DDTHH:mm:ssZ`. Quindi utilizza la funzione `toString` specificando il formato desiderato, che in questo caso è `%d/%m/%Y` per ottenere una stringa nel formato giorno/mese/anno. Se stampiamo il valore di `myString`, otterremo come output "29/10/2019".

## Approfondimento

La funzione `toString` accetta un'opzione di formato specifico che ti permette di personalizzare ulteriormente la stringa in output. Ad esempio, puoi utilizzare `%b` per ottenere il mese in tre lettere o `%B` per ottenere il mese completo. Puoi anche combinare diversi formati utilizzando una barra " /. Ad esempio, `%d/%B/%Y` produrrà una stringa come "29/Ottobre/2019".

## Vedi Anche

- Documentazione di Elm sul modulo `Date`: https://package.elm-lang.org/packages/elm/core/latest/Date
- Formattazione delle date in Elm: https://package.elm-lang.org/packages/justinmimbs/date-extra/latest/Date-Format