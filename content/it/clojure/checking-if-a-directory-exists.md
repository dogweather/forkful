---
title:    "Clojure: Verifica dell'esistenza di una directory"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Spesso, quando si scrive un programma, è importante sapere se una determinata directory esiste o meno. Questo può essere utile per automatizzare alcune operazioni come la creazione di file o la gestione delle risorse.

## Come Fare 

Per verificare l'esistenza di una directory in Clojure, possiamo utilizzare la funzione `clojure.java.io/file` seguita dalla funzione `exists?`. Questo ci restituirà un valore booleano, `true` se la directory esiste e `false` se non esiste. Ecco un esempio di codice:

```Clojure
(def directory "caminho/della/cartella")

(if (clojure.java.io/file directory)
  (println "La directory esiste.")
  (println "La directory non esiste."))
```

L'output di questo esempio sarà "La directory esiste" se la directory specificata esiste nel percorso specificato, altrimenti l'output sarà "La directory non esiste".

## Approfondimento

È interessante notare che la funzione `exists?` non verifica solo l'esistenza di una directory, ma può anche essere utilizzata per verificare l'esistenza di un file. Inoltre, possiamo combinare la funzione `exists?` con altre funzioni di Clojure, come ad esempio `dir?` per verificare se un percorso specificato corrisponde a una directory o a un file. Ad esempio:

```Clojure
(def directory "caminho/della/cartella")

(if (and (clojure.java.io/file directory) (dir? directory))
  (println "Il percorso specificato corrisponde a una directory.")
  (println "Il percorso specificato non corrisponde a una directory."))
```

Se il percorso specificato corrisponde effettivamente a una directory, l'output sarà "Il percorso specificato corrisponde a una directory", altrimenti l'output sarà "Il percorso specificato non corrisponde a una directory".

## Vedi Anche 

- [ClojureDocs: clojure.java.io/file](https://clojuredocs.org/clojure.java.io/file)
- [The Clojure Programming Language](https://clojure.org/)