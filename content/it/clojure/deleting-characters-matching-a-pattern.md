---
title:                "Clojure: Eliminazione di caratteri corrispondenti a un modello."
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Ci sono diversi motivi per cui potresti voler cancellare i caratteri corrispondenti a un determinato modello nel tuo codice Clojure. Potresti voler rimuovere caratteri non desiderati da una stringa per ottenere un output pulito, o potresti voler eliminare caratteri speciali prima di elaborare i dati.

## Come

Ecco un esempio di come puoi utilizzare la funzione `remove` per eliminare caratteri corrispondenti a un modello specifico in una stringa:

```Clojure
(remove #"[aeiou]" "ciao mondo") 
```

Questo codice restituirà "c mnd", poiché ha rimosso tutte le vocali dalla stringa. È possibile utilizzare espressioni regolari per specificare il modello dei caratteri che si desidera eliminare.

## Deep Dive

La funzione `remove` accetta un modello di espressione regolare come primo argomento e la stringa su cui lavorare come secondo argomento. È in grado di rimuovere tutti i caratteri corrispondenti al modello dalla stringa e restituire la stringa risultante.

Inoltre, è possibile utilizzare la funzione `replace` per sostituire i caratteri corrispondenti a un modello con un altro carattere o stringa. Ad esempio:

```Clojure
(replace #"[^a-z0-9 ]" "" "Ciao! Questa stringa ha caratteri speciali!") 
```

Questo codice restituirà "Ciao Questa stringa ha caratteri speciali", poiché ha sostituito tutti i caratteri speciali con una stringa vuota.

## Vedi anche

- [Documentazione ufficiale di Clojure](https://clojure.org/reference/strings)
- [Tutorial sulle espressioni regolari in Clojure](https://clojure.org/reference/regexp)
- [Esempi di utilizzo di funzioni di manipolazione delle stringhe in Clojure](https://github.com/seancorfield/clojure-string-tour)