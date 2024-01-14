---
title:    "Clojure: Creazione di un file temporaneo"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare file temporanei è una tecnica comune utilizzata nella programmazione per eseguire operazioni temporanee e gestire dati temporanei in modo efficiente.

## Come fare

Per creare un file temporaneo in Clojure, è possibile utilizzare la funzione `with-open` combinata con la funzione `temp-file` del pacchetto `java.io.File`. Vediamo un esempio:

```Clojure
(with-open [temp-file (java.io.File/createTempFile "nomefile" ".txt")]
  (println "Il file temporaneo è stato creato con successo.")
  (println "Il percorso del file è:" (.getAbsolutePath temp-file))
  (println "Il nome del file è:" (.getName temp-file)))
```

Esempio di output:

```
Il file temporaneo è stato creato con successo.
Il percorso del file è: /var/folders/rh/3vn7w2jx2kv28fb8ywc5tf6w0000gn/T/nomefile7944570293349999284.txt
Il nome del file è: nomefile7944570293349999284.txt
```

In questo esempio, stiamo utilizzando la funzione `createTempFile` per creare un file temporaneo con il nome "nomefile" e l'estensione ".txt". Il file viene quindi aperto e utilizzato all'interno dello scope della funzione `with-open`, che garantirà che il file venga chiuso automaticamente una volta finito l'utilizzo.

## Approfondimento

La funzione `temp-file` del pacchetto `java.io.File` accetta due argomenti facoltativi: il percorso della directory in cui creare il file temporaneo e un prefisso da aggiungere al nome del file temporaneo. Per impostazione predefinita, il percorso della directory sarà la directory di sistema temporanea e il prefisso sarà "nso".

Inoltre, è possibile utilizzare la funzione `temp-file` per creare più file temporanei all'interno di una singola funzione, specificando un prefisso diverso per ogni file.

## Vedi anche

- [Documentazione di Clojure su createTempFile](https://clojuredocs.org/clojure.java.io/createTempFile)
- [Tutorial su file temporanei in Clojure](https://rundis.github.io/blog/2017/clojure-repeating-tasks-asynchronously-using-future)
- [Esempio di utilizzo di with-open in Clojure](https://clojuredocs.org/clojure.core/with-open)