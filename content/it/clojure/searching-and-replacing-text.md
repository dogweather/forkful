---
title:                "Clojure: Ricerca e sostituzione di testo"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

C'è un motivo per cui la sostituzione di testo è una delle attività più comuni nella programmazione. È necessario modificare un codice sorgente, aggiornare un documento o semplicemente correggere un errore di ortografia. Indipendentemente dal motivo, la capacità di cercare e sostituire automaticamente il testo è un'abilità essenziale per ogni programmatore.

## Come fare

Fortunatamente, Clojure offre una sintassi semplice e potente per effettuare operazioni di sostituzione del testo. Vediamo alcuni esempi che mostrano come si utilizza la funzione `replace`:

```
Clojure
(replace "cane" "gatto" "Mi piace il mio cane!")
;; Output: "Mi piace il mio gatto!"
```

In questo esempio, stiamo sostituendo la parola "cane" con "gatto" nella stringa "Mi piace il mio cane!". La funzione `replace` accetta tre parametri: il testo da cercare, il testo da sostituire e la stringa in cui effettuare la sostituzione.

```
Clojure
(replace #"([A-Z]+) ([0-9]+)" "$2 $1" "ALFA 123")
;; Output: "123 ALFA"
```

In questo secondo esempio, stiamo utilizzando un'espressione regolare per invertire l'ordine delle parole e delle cifre nella stringa "ALFA 123". La funzione `replace` accetta anche espressioni regolari come parametro di ricerca.

## Approfondimento

Oltre alla semplice funzione `replace`, Clojure dispone di una vasta gamma di funzioni per la manipolazione del testo. Ad esempio, la funzione `replace-first` sostituisce solo la prima occorrenza trovata, mentre `replace-regexp` permette di sostituire in base a un'espressione regolare. Inoltre, si può utilizzare il metodo `replaceAll` delle stringhe Java per sostituire in modo case-insensitive.

È importante notare che le stringhe in Clojure sono immutabili, quindi ogni operazione di sostituzione restituisce una nuova stringa. Inoltre, Clojure utilizza una libreria chiamata `clojure.string` per le operazioni di manipolazione del testo, quindi è necessario importarla prima di utilizzare queste funzioni.

## Vedi anche

- [La documentazione ufficiale di `clojure.string`](https://clojure.github.io/clojure/clojure.string-api.html)
- [Il tutorial su espressioni regolari di Clojure](https://clojuredocs.org/clojure.core/re-find)

 Grazie per aver letto questo articolo. Speriamo che ti sia stato utile nella tua avventura di programmazione con Clojure!# Vedi anche

- [La documentazione ufficiale di `clojure.string`](https://clojure.github.io/clojure/clojure.string-api.html)
- [Il tutorial su espressioni regolari di Clojure](https://clojuredocs.org/clojure.core/re-find)