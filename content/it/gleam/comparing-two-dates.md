---
title:                "Gleam: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Comparare due date è una parte essenziale della programmazione in Gleam. Sapere come farlo ti permette di gestire in modo efficiente le date nel tuo codice e di ottenere sempre i risultati desiderati.

## Come Fare

Per comparare due date in Gleam, è necessario prima creare due variabili di tipo `Date`. Puoi farlo utilizzando la funzione `Date.new`.

```Gleam
let prima_data = Date.new(2021, 12, 25)
let seconda_data = Date.new(2022, 1, 1)
```

Una volta che hai le tue due date, puoi utilizzare l'operatore di uguaglianza `==` per confrontarle. Ecco un esempio che verifica se le due date sono uguali.

```Gleam
if prima_data == seconda_data {
  io.print("Le due date sono uguali")
} else {
  io.print("Le due date sono diverse")
}
```

Se vuoi invece sapere quale delle due date è antecedente, puoi utilizzare l'operatore `<` o `>`. Ecco un esempio che confronta le due date e stampa il risultato.

```Gleam
if prima_data < seconda_data {
  io.print("La prima data è antecedente alla seconda")
} else {
  io.print("La seconda data è antecedente alla prima")
}
```

## Approfondimento

Per confrontare due date in modo più preciso, puoi utilizzare la funzione `Date.compare` che restituisce un valore `Ordering` che può essere `Less`, `Equal` o `Greater`. Questo ti permette di gestire anche casi in cui le due date sono molto simili.

```Gleam
let prima_data = Date.new(2021, 12, 25, 12, 30, 0)
let seconda_data = Date.new(2021, 12, 25, 12, 30, 30)

let ordine = Date.compare(prima_data, seconda_data)

if ordine == Ordering.Equal {
  io.print("Le due date sono uguali")
} else if ordine == Ordering.Less {
  io.print("La prima data è precedente alla seconda")
} else {
  io.print("La seconda data è precedente alla prima")
}
```

## Vedi Anche

- Documentazione ufficiale di Gleam sulle date
- Tutorial su come manipolare le date in Gleam
- Esempi di codice su GitHub che utilizzano la manipolazione delle date