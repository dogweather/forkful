---
title:                "Gleam: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Sempre è utile sapere come calcolare una data futura o passata per pianificare eventi o riunioni. Con Gleam, è un gioco da ragazzi!

## Come Fare

Per calcolare una data in futuro o passato, possiamo utilizzare la funzione `Date.add` o `Date.subtract`. Ad esempio, se vogliamo aggiungere 3 giorni alla data corrente, possiamo scrivere:

```Gleam
let data_corrente = Date.now()
let data_futura = Date.add(data_corrente, 3, Date.Unit.Day)

Debug.to_string(data_futura) // Output: "2021-11-08T09:00:00Z"
```

Allo stesso modo, se vogliamo sottrarre 2 settimane dalla data corrente, possiamo scrivere:

```Gleam
let data_corrente = Date.now()
let data_passata = Date.subtract(data_corrente, 2, Date.Unit.Week)

Debug.to_string(data_passata) // Output: "2021-10-25T09:00:00Z"
```

Possiamo anche specificare l'unità di tempo desiderata come terzo argomento della funzione, come mostrato negli esempi sopra.

## Approfondimento

Gleam offre anche altre funzioni per manipolare le date, come `Date.diff` per calcolare la differenza tra due date e `Date.compare` per confrontare due date. Inoltre, è possibile specificare il fuso orario nella creazione di una nuova data utilizzando `Date.from_timestamp` o `Date.from_fields`.

## Vedi Anche

- [Documentazione su Date in Gleam](https://gleam.run/documentation/std/datetime/)
- [Tutorial su Gleam per principianti](https://medium.com/@gleamlang/gleam-language-building-an-api-f1c2b7909b96)
- [Esempi di progetti in Gleam](https://github.com/gleam-lang/awesome-gleam)