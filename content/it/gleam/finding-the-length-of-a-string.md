---
title:                "Gleam: Trovare la lunghezza di una stringa."
simple_title:         "Trovare la lunghezza di una stringa."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Trovare la lunghezza di una stringa può sembrare una semplice operazione, ma in realtà può essere utile per una varietà di scopi. Ad esempio, se si sta lavorando su un'applicazione di gestione di una lista della spesa, potrebbe essere necessario controllare la lunghezza di una stringa inserita dall'utente per assicurarsi che non superi la lunghezza massima consentita. Quindi, capire come trovare la lunghezza di una stringa può essere un'abilità molto utile per un programmatore.

## Come Fare

Per trovare la lunghezza di una stringa, possiamo utilizzare la funzione `String.length()` in Gleam. Vediamo un esempio pratico:

```Gleam
let stringa = "Salve"
let lunghezza = String.length(stringa)
IO.println("La lunghezza di 'Salve' è ${lunghezza}.")
```

Questo codice stamperà "La lunghezza di 'Salve' è 5." poiché ci sono 5 caratteri nella stringa "Salve". Nota che dobbiamo assegnare il valore restituito da `String.length()` a una variabile per poterlo utilizzare successivamente.

Altre funzioni utili per lavorare con le stringhe in Gleam includono `String.slice()` per estrarre una porzione di una stringa e `String.reverse()` per invertire l'ordine dei caratteri. Sperimenta con queste funzioni e vedi cosa riesci a creare!

## Approfondimento

È interessante notare che, internamente, la lunghezza di una stringa in Gleam è rappresentata come una lista di caratteri `Char.list`. Questo significa che, mentre per noi le stringhe sembrano essere un unico valore, in realtà sono composte da una serie di caratteri separati. Questo è importante da tenere a mente quando si lavora con le stringhe e si desidera modificarne i contenuti.

## Vedi Anche

- [Documentazione su Stringhe in Gleam](https://gleam.run/book/std/string.html)
- [Altro esempio di utilizzo di `String.length()`](https://stackoverflow.com/questions/60037188/how-to-get-the-length-of-a-string-in-gleam)