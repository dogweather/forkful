---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Che Cosa e Perché?

L'estrazione di sottostringhe è la pratica di ottenere piccoli segmenti di testo da una stringa più grande. È un'operazione comune nella programmazione, utilizzata per manipolare e analizzare i dati del testo.

## Come Fare:

In Clojure, usiamo la funzione `subs` per estrarre le sottostringhe. Prende due argomenti: la stringa e l'indice d'inizio. Oppure, possiamo fornire un indice di fine come terzo argomento.
```Clojure
(let [frase "Ciao, Mondo"]
  (subs frase 0 4))  
```
Questo darà come output: "Ciao".

Se non forniamo l'indice di fine, l'estrazione andrà fino alla fine della stringa.
```Clojure
(let [frase "Ciao, Mondo"]
  (subs frase 5))
```
Questo darà come output: "Mondo".

## Approfondimento

La funzione `subs` è una parte fondamentale di Clojure da quando è stata introdotta nel 2007. Anche se ci sono biblioteche esterne che offrono funzionalità simili, `subs` è una soluzione semplice e diretta che non richiede dipendenze aggiuntive.

All'interno, `subs` crea un nuovo oggetto stringa anziché referenziare la stringa originale. Questo può avere un impatto sulla performance se stai lavorando con stringhe enormi, ma nella maggior parte dei casi non dovrebbe essere un problema.

Alternativamente, per stringhe molto grandi, potrebbe essere più efficiente usare un approccio basato su stream. Questo, però, potrebbe richiedere più codice e non è supportato nativamente in Clojure.

## Vedi Anche

1. [Clojure Documentation - `subs`](https://clojuredocs.org/clojure.core/subs)
2. [Clojure School - Working with Strings](https://clojure.org/community/resources#_clojure_from_the_ground_up)