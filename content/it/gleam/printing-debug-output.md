---
title:    "Gleam: Stampare l'output di debug"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Perché

Spesso, durante la scrittura di codice, ci troviamo ad affrontare situazioni complesse e difficili da capire. Una delle tecniche più utili per superare queste sfide è la stampa di output di debug. Vediamo perché.

## Come farlo

Per stampare output di debug in Gleam, possiamo utilizzare la funzione `debug.inspect`. Ad esempio, possiamo stampare il valore di una variabile utilizzando il seguente codice:

```Gleam
let my_var = "Hello World"
debug.inspect(my_var)
```

Questo codice produrrà il seguente output di debug:

```
"Hello World"
```

Possiamo anche stampare più variabili contemporaneamente, utilizzando una lista di elementi da stampare come argomento della funzione `debug.inspect`:

```Gleam
let var_1 = 42
let var_2 = "ciao"
let var_3 = true

debug.inspect([var_1, var_2, var_3])
```

Questo codice ci darà il seguente output di debug:

```
42
"ciao"
true
```

## Approfondimento

La stampa di output di debug può essere particolarmente utile durante la fase di sviluppo del software, quando siamo ancora in fase di debugging e risoluzione dei bug. Possiamo utilizzare questa tecnica per visualizzare il valore delle variabili in un dato momento del codice, per controllare se sono corretti o se ci sono problemi con la loro assegnazione.

Inoltre, utilizzando la funzione `debug.inspect`, possiamo anche stampare strutture dati più complesse come liste, mappe o record. Questo ci permette di esplorare e comprendere meglio i nostri dati durante la fase di sviluppo.

## Vedi anche

- Documentazione ufficiale di Gleam: https://gleam.run/
- Tutorial su come stampare output di debug: https://gleam.run/book/tour/debug.html
- Video tutorial su Gleam di Codecademy: https://www.youtube.com/watch?v=1NUr7nsWHD4