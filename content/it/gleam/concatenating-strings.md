---
title:                "Gleam: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Hai mai incontrato una situazione in cui hai bisogno di unire due o più stringhe in un'unica stringa? Forse stai creando un sistema di gestione delle scorte e devi unire il nome del prodotto con la quantità disponibile per visualizzarlo come un'unica stringa. O forse stai creando un gioco e devi unire il nome del personaggio con il suo livello per mostrare al giocatore. Indipendentemente dal motivo, concatenare le stringhe è un'operazione molto comune nella programmazione e con questo post imparerai come farlo in Gleam.

## Come fare

In Gleam, per concatenare le stringhe, possiamo utilizzare l'operatore `++`. Questo operatore ci consente di unire due o più stringhe in una sola stringa. Ecco un esempio:

```Gleam
let nome_prodotto = "Maglietta"
let disponibilita = 10
IO.println("Ci sono " ++ disponibilita ++ " " ++ nome_prodotto ++" disponibili")
```

Questo darà come output:

> Ci sono 10 Magliette disponibili

Possiamo anche concatenare più di due stringhe, semplicemente aggiungendo più operatori `++`. Ad esempio:

```Gleam
let nome = "Giovanni"
let cognome = "Bianchi"
let saluto = "Ciao, mi chiamo " ++ nome ++ " " ++ cognome
IO.println(saluto)
```

Questo darà come output:

> Ciao, mi chiamo Giovanni Bianchi

## Approfondimento

Grazie all'operatore `++` possiamo anche concatenare le stringhe con altri tipi di dato come numeri o booleani. Tuttavia, dobbiamo prestare attenzione al tipo di dati che stiamo unendo, poiché Gleam è fortemente tipizzato e richiede che i tipi siano compatibili. Ad esempio, non possiamo unire una stringa con un numero direttamente, ma possiamo convertire il numero in una stringa utilizzando il modulo `String` e poi concatenarlo.

Ecco un esempio di concatenazione di una stringa con un numero:

```Gleam
let nome = "Maria"
let eta = 25
let presentazione = "Ciao, mi chiamo " ++ nome ++ " e ho " ++ String.from_int(age) ++ " anni"
IO.println(presentazione)
```

Questo darà come output:

> Ciao, mi chiamo Maria e ho 25 anni

## Vedi anche

- [Documentazione ufficiale di Gleam sulle stringhe](https://gleam.run/book/core-stdlib#Strings)
- [Come utilizzare gli operatori in Gleam](https://gleam.run/blog/operator-precedence)
- [Tutorial su come utilizzare stringhe in Gleam](https://www.tutorialspoint.com/gleam/gleam_strings.htm)