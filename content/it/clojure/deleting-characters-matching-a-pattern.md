---
title:                "Eliminare i caratteri corrispondenti a un modello"
html_title:           "Clojure: Eliminare i caratteri corrispondenti a un modello"
simple_title:         "Eliminare i caratteri corrispondenti a un modello"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Cancellare i caratteri che corrispondono ad un certo modello è un'operazione comune tra i programmatori. Si utilizza per rimuovere parti indesiderate da una stringa, come ad esempio caratteri di punteggiatura o spazi vuoti. Ciò può rendere più efficiente il codice e migliorare le prestazioni del programma.

## Come fare:
Una delle opzioni più semplici per eliminare i caratteri corrispondenti ad un modello in Clojure è utilizzare la funzione ```clojure.string/replace``` e specificare il modello e la stringa da sostituire con una stringa vuota. Ad esempio, per eliminare tutti i caratteri di punteggiatura da una stringa si può utilizzare il seguente codice:

```
(clojure.string/replace "Ciao, come stai?" #"\p")
```

Questo restituirà la stringa "Ciao comestai". Nota che il modello ```\p``` corrisponde a tutti i caratteri di punteggiatura.

## Approfondimento:
Esistono anche altre opzioni per eliminare i caratteri corrispondenti ad un modello in Clojure. Una di queste è utilizzare la funzione ```clojure.string/replace-first```, che elimina solo la prima occorrenza del modello nella stringa. Inoltre, è possibile utilizzare la funzione ```clojure.string/replace-regex``` per sostituire il modello con una stringa diversa invece di una stringa vuota.

## Vedi anche:
- Documentazione ufficiale sulla funzione ```clojure.string/replace```: https://clojuredocs.org/clojure.string/replace
- Altro esempio di utilizzo della funzione ```clojure.string/replace```: https://clojure.org/guides/data_processing