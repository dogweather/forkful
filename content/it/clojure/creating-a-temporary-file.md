---
title:                "Clojure: Creare un file temporaneo"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo può essere utile quando è necessario salvare temporaneamente dati o risultati di un programma senza doverli memorizzare permanentemente sul computer. Questo può essere particolarmente utile durante lo sviluppo di un'applicazione o lo scripting di un processo.

## Come fare

Per creare un file temporaneo in Clojure, è possibile utilizzare la funzione `with-open`. Questa funzione crea automaticamente un file temporaneo, esegue il codice all'interno del suo corpo e poi lo elimina quando il corpo è terminato. Vediamo un esempio pratico:

```Clojure
(with-open [temp-file (java.io.File/createTempFile "my_temp_file" ".txt")]
  (println "Sto scrivendo nel file temporaneo.")
  (clojure.java.io/append temp-file "Questo è il mio primo file temporaneo!")
  (println "Il contenuto del file temporaneo è:")
  (println (slurp temp-file)))
```

Output:

```
Sto scrivendo nel file temporaneo.
Il contenuto del file temporaneo è:
Questo è il mio primo file temporaneo!
```

Come si può vedere nell'esempio, il file temporaneo viene creato utilizzando la funzione `java.io.File/createTempFile` con un nome e una estensione specificati. Successivamente, il codice all'interno del corpo di `with-open` viene eseguito e il contenuto viene scritto nel file temporaneo utilizzando la funzione `clojure.java.io/append`. Infine, il contenuto del file temporaneo viene letto con `slurp` e stampato a schermo.

## Approfondimento

Oltre alla funzione `with-open`, è possibile creare un file temporaneo utilizzando la libreria `clojure.java.io` direttamente. Ad esempio:

```Clojure
(def temp-file (clojure.java.io/file (clojure.java.io/tmpdir) "my_temp_file.txt"))
```

Questa linea di codice crea un oggetto `java.io.File` che rappresenta un file temporaneo all'interno della directory dei file temporanei del sistema operativo. È importante notare che il file non viene creato immediatamente, ma solo quando viene utilizzato in una funzione di scrittura come `clojure.java.io/output-stream`.

## Vedi anche

- [ClojureDocs - with-open](https://clojuredocs.org/clojure.core/with-open) 
- [ClojureDocs - java.io.File/createTempFile](https://clojuredocs.org/clojure.java.io/file/createTempFile)
- [ClojureDocs - clojure.java.io/output-stream](https://clojuredocs.org/clojure.java.io/output-stream)