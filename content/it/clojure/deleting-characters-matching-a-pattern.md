---
title:                "Clojure: Eliminazione dei caratteri corrispondenti a un determinato modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un determinato modello"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per voler eliminare caratteri che corrispondono ad uno specifico modello in Clojure. Ad esempio, potresti voler pulire una stringa di input da caratteri indesiderati prima di elaborarla, o rimuovere caratteri non validi da un file di testo. Eliminare caratteri in questo modo è un'operazione comune in molte applicazioni di programmazione.

## Come 
Per eliminare caratteri in Clojure, puoi utilizzare la funzione `replace` in combinazione con espressioni regolari. Ecco un esempio di codice che rimuove tutti i numeri da una stringa:

```Clojure
(def input "abc123def456ghi789")
(replace #"\d+" input "")
```
Output: "abcdefghi"

In questo esempio, l'espressione regolare `#"\d+"` corrisponde a tutti i numeri presenti nella stringa di input e la funzione `replace` li sostituisce con una stringa vuota, eliminandoli così dalla stringa.

Un'altra opzione è utilizzare la funzione `filter` combinata con la negazione dell'espressione regolare. Ecco un esempio di codice che elimina tutti i caratteri non alfabetici da una stringa:

```Clojure
(def input "ab#c$d%e&f")
(filter #(re-find #"[a-zA-Z]" (str %)) input)
```
Output: "abcdef"

In questo esempio, l'espressione regolare `#"[a-zA-Z]"` corrisponde a tutti i caratteri alfabetici presenti nella stringa di input e `filter` restituisce solo i caratteri che corrispondono a quella espressione regolare, eliminando così tutti i caratteri non alfabetici.

## Approfondimento
Oltre all'utilizzo delle espressioni regolari, ci sono altre opzioni per eliminare caratteri in Clojure. Ad esempio, puoi utilizzare la funzione `replace-first` per rimuovere solo il primo carattere corrispondente a un modello, anziché tutti i caratteri.

Inoltre, è importante notare che le operazioni di eliminazione dei caratteri sono spesso eseguite per motivi di sicurezza. Ad esempio, se stai elaborando una stringa di input fornita dall'utente, è importante rimuovere caratteri potenzialmente dannosi o invalidi prima di utilizzare la stringa nel tuo programma. Invece di specificare manualmente quali caratteri rimuovere, puoi utilizzare librerie come clojure.string per eliminare automaticamente tutti i caratteri non sicuri da una stringa.

## Vedi anche
- [Documentazione ufficiale su le espressioni regolari in Clojure](https://clojure.org/reference/reader#_regular_expression_syntax)
- [Tutorial su clojure.string](https://clojuredocs.org/clojure.string)
- [Spiegazione delle principali funzioni di stringhe in Clojure](https://beginners-clojure.com/docs/basic-strings/)