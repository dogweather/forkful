---
title:    "Gleam: Cancellazione di caratteri corrispondenti ad un modello"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler eliminare alcuni caratteri da una stringa in Gleam. Ad esempio, potresti avere una stringa che contiene alcuni caratteri di spazio vuoti che vuoi rimuovere prima di poterla utilizzare correttamente. Oppure potresti dover eliminare tutti i caratteri non alfanumerici da una stringa per scopi di sanitizzazione dei dati. In questo articolo, ti mostreremo come eliminare facilmente i caratteri che corrispondono a un determinato modello in Gleam.

## Come

Per eliminare i caratteri che corrispondono a un modello, possiamo utilizzare la funzione `String.filter` di Gleam. Prendiamo ad esempio una stringa di parole separate da virgole come "apple, banana, orange, grapefruit". Vogliamo eliminare tutte le virgole da questa stringa. Ecco un codice di esempio che possiamo utilizzare:

```Gleam
import gleam/string

let words = "apple, banana, orange, grapefruit"

let filtered_words =
  String.filter(\c -> c != ',', words)
```

In questo codice, stiamo usando `gleam/string` per accedere alla funzione `String.filter`, che prende in input una funzione di filtro e una stringa. La funzione di filtro prende un carattere e restituisce un valore booleano. Se il valore è `true`, il carattere viene incluso nella nuova stringa, altrimenti viene ignorato. Nel nostro esempio, stiamo usando `\c` come alias per il carattere in input e stiamo verificando se è diverso da una virgola. Se è così, viene incluso nella nuova stringa `filtered_words`.

Se eseguiamo questo esempio nel nostro programma, otterremo come output la stringa "apple banana orange grapefruit", senza le virgole.

## Deep Dive

Oltre alla funzione `String.filter`, esistono anche altre opzioni per eliminare i caratteri che corrispondono a un modello in Gleam. Una di queste è la funzione `String.delete`, che prende in input una stringa di caratteri da eliminare. Utilizzando l'esempio precedente, possiamo riscrivere il codice come segue:

```Gleam
import gleam/string

let words = "apple, banana, orange, grapefruit"

let filtered_words =
  String.delete(", ", words)
```

In questo codice, stiamo utilizzando la funzione `String.delete` per eliminare tutti i caratteri "," e " " dalla stringa `words`, che è equivalente al nostro esempio precedente. Tuttavia, con questa funzione possiamo specificare più di un carattere da eliminare, fornendoli come seconda stringa di input.

## Vedi anche

- [Documentazione delle stringhe di Gleam](https://gleam.run/api/gleam_stdlib/0.11.0/String.html)
- [Tutorial sulle funzioni di stringa di Gleam](https://gleam.run/tutorials/strings)

Conoscere le funzioni di Gleam per eliminare i caratteri che corrispondono a un modello ti sarà utile in molte situazioni in cui devi manipolare le stringhe. Speriamo che questo articolo ti sia stato utile e ti abbia fornito una conoscenza più approfondita di Gleam. Continua a esplorare la documentazione e i tutorial per scoprire le molte altre funzionalità di questo linguaggio di programmazione funzionale. Grazie per aver letto!