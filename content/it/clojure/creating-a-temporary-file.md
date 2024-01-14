---
title:    "Clojure: Creazione di un file temporaneo"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Perché: La creazione di file temporanei in Clojure
La creazione di file temporanei può essere utile in molti casi, ad esempio quando si vuole manipolare dati temporanei senza dover utilizzare file permanenti o quando si vuole garantire la privacy dei dati.

## Come fare: Utilizzare la funzione "with-tempfile"
In Clojure, è possibile creare un file temporaneo utilizzando la funzione "with-tempfile". Questa funzione accetta due parametri: un prefisso per il nome del file e una funzione che verrà eseguita con il file temporaneo come argomento. Ad esempio:

```Clojure
(with-tempfile "temp-" (fn [file]
  (println "Il file temporaneo creato è:" file)))
```

L'output di questo codice sarà qualcosa del tipo: "Il file temporaneo creato è: /var/folders/9c/jz6syh9539ldkvr9y30h\_lc40000gn/T/temp-7365937472430203967.txt"

## Approfondimento: Utilizzare il namespace "clojure.java.io"
Per creare un file temporaneo utilizzando il namespace "clojure.java.io", è possibile utilizzare la funzione "make-temp-file". Questa funzione accetta i seguenti parametri opzionali: prefisso, suffisso, directory e istante di creazione. Ad esempio:

```Clojure
(require '[clojure.java.io :as io])

(io/make-temp-file "file" ".txt" "/var/tmp/" "creazione")
```

L'output di questo codice sarà un file con il nome "file-creazione.txt" nella directory /var/tmp/.

# Vedi anche
- Documentazione "with-tempfile": https://clojuredocs.org/clojure.core/with-tempfile
- Documentazione "make-temp-file": https://clojuredocs.org/clojure.java.io/make-temp-file