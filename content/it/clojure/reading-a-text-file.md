---
title:    "Clojure: Leggere un file di testo"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Perché

Lettura di un file di testo è un'operazione fondamentale in molti programmi Clojure. Questo metodo permette di accedere e manipolare i dati contenuti in un file, sia esso un documento di testo, un CSV o un log. È un'abilità importante per qualsiasi programmatore che lavora con dati.

## Come Fare

Per iniziare, dobbiamo prima importare il modulo `java.io`: 

```Clojure
(import '[java.io BufferedReader File FileReader])
``` 

Utilizzeremo la funzione `java.io.FileReader` per aprire un'istanza della classe `java.io.File`, che rappresenta il nostro file di testo. Usa il seguente codice come esempio:

```Clojure
(let [file (new File "test_file.txt")
      reader (new BufferedReader (new FileReader file))
```

Ora possiamo leggere il file e immagazzinare il suo contenuto in una variabile `file-content` utilizzando il metodo `readLine` del nostro oggetto `reader`:

```Clojure
(with-open [rdr (reader file)]
  (def file-content (->> (line-seq rdr))))
```

Possiamo anche scrivere dei filtri per il nostro contenuto del file, come per esempio un filtro per restituire solo le linee che iniziano con un certo carattere:

```Clojure
(defn starting-with [ch]
  #(= (first %) ch))

(def filtered-file-content
  (filter (starting-with \B) file-content))
```

E il risultato sarà il seguente:

```Clojure
("Buongiorno mondo"
"Benvenuti a Clojure"
"Buona giornata")
```

## Approfondimento

La funzione `readLine` legge una linea di testo dal nostro file, mentre `line-seq` legge il contenuto del file come una serie di linee, restituendole come una sequenza di stringhe. Usando `def` all'interno di `with-open`, garantiamo che il nostro oggetto `rdr` verrà chiuso dopo che `file-content` è stato definito, in modo da evitare eventuali problemi di memoria.

Per aggiungere del testo a un file, dobbiamo prima convertire il nostro contenuto in una stringa e poi utilizzare il metodo `write` dell'oggetto `FileWriter`:

```Clojure
(let [file-to-write (new FileWriter "test_file.txt" true)]
	(.write file-to-write "Buon pomeriggio, mondo!")
    (.close file-to-write))
```

Nota che abbiamo impostato il secondo parametro a `true` per dire a Clojure che vogliamo aggiungere del testo al file esistente anziché sovrascriverlo.

## Vedi Anche

Per informazioni più approfondite sulla lettura e scrittura di file in Clojure puoi consultare:

- [Documentazione Java I/O](https://docs.oracle.com/javase/tutorial/essential/io/)
- [Clojure I/O](https://clojure.org/reference/io)