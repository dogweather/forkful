---
title:                "Gleam: Confrontare due date"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

La comparazione di due date è uno strumento importante per i programmatori, in quanto consente di verificare facilmente la relazione temporale tra due eventi o dati.

## Come fare

Per confrontare due date in Gleam, è possibile utilizzare la funzione `DateTime.compare()` che accetta due argomenti di tipo `DateTime` e restituisce un valore `Ordering`. Questo è un tipo di dato che rappresenta la relazione tra due valori e può essere `Less`, `Equal` o `Greater` a seconda che il primo valore sia rispettivamente precedente, uguale o successivo al secondo.

Ecco un esempio di codice che utilizza la funzione `DateTime.compare()`:

```Gleam
let now = DateTime.now()
let birthday = DateTime.make(1990, April, 25)

let relationship = DateTime.compare(now, birthday)

case relationship {
    Equal -> "Today is your birthday!"
    Less -> "Your birthday has passed."
    Greater -> "Your birthday is still to come."
}
```

L'output del codice sopra sarà `Your birthday has passed.` se il giorno di oggi è successivo al 25 aprile 1990.

## Approfondimenti

È importante notare che la funzione `DateTime.compare()` non tiene conto del fuso orario, ma confronta solo le date nella stessa scala temporale. Inoltre, è possibile utilizzare altri metodi come `DateTime.add()`, `DateTime.sub()` e `DateTime.diff()` per manipolare le date e ottenere informazioni più dettagliate sulla relazione temporale tra di esse.

## Vedi anche

- [La documentazione di Gleam sulla data e l'ora](https://gleam.run/documentation/stdlib/datetime/)
- [Un articolo su come gestire le date in Gleam](https://medium.com/@gleamlang/how-to-handle-dates-in-gleam-355fe4b39d4e)