---
title:                "Scribendo un file di testo"
html_title:           "Clojure: Scribendo un file di testo"
simple_title:         "Scribendo un file di testo"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Questa guida è rivolta a coloro che vogliono imparare a creare un file di testo utilizzando Clojure. Scrivere un file di testo può essere utile per salvare dati o output di un programma in un formato leggibile. Inoltre, è un'ottima opportunità per imparare a creare e gestire file utilizzando Clojure.

## Come fare

Innanzitutto, apriamo il nostro ambiente di sviluppo Clojure preferito. Possiamo utilizzare anche un editor di testo normale, ma per questo tutorial useremo l'interfaccia utente del REPL di Clojure per facilità di esempio.

### Creare un file di testo

Per creare un file di testo utilizziamo la funzione `spit` che accetta due argomenti: il percorso del file (incluso il nome) e il contenuto del file. Ad esempio:

```Clojure
(spit "mio_file.txt" "Ciao mondo!")
```

Questo creerà un nuovo file di testo chiamato "mio_file.txt" nella stessa directory del nostro progetto e inserirà il testo "Ciao mondo!" al suo interno. Possiamo utilizzare qualsiasi percorso valido come primo argomento, quindi possiamo scegliere anche una posizione specifica sul nostro computer.

È importante notare che la funzione `spit` sovrascriverà qualsiasi file esistente con lo stesso nome, quindi assicurati di non utilizzare il nome di un file già esistente.

### Aggiungere contenuto a un file esistente

Se vogliamo aggiungere del contenuto a un file di testo esistente, possiamo utilizzare la funzione `append` invece di `spit`. Ad esempio:

```Clojure
(append "mio_file.txt" "Questo è un nuovo testo!")
```

Questo aggiungerà la stringa "Questo è un nuovo testo!" alla fine del file "mio_file.txt".

### Leggere il contenuto di un file di testo

Per leggere il contenuto di un file di testo, possiamo utilizzare la funzione `slurp` che accetta un unico argomento, il percorso del file. Ad esempio:

```Clojure
(slurp "mio_file.txt")
```

Questa funzione restituirà tutto il contenuto del file come una stringa. Possiamo anche utilizzare la funzione `clojure.string/split` per dividere il contenuto in una sequenza di righe. Ad esempio:

```Clojure
(clojure.string/split (slurp "mio_file.txt") #"\n")
```

Questo restituirà una sequenza contenente ogni riga del file come un elemento.

## Approfondiamo

Oltre alle funzioni sopra menzionate, Clojure offre anche una varietà di pacchetti e librerie per lavorare con file di testo, come ad esempio ClojureCSV e flatland/useful, che rendono il processo di scrittura e lettura di file molto più semplice e flessibile. È importante anche comprendere come gestire gli errori di file, come il caso in cui il file non esiste o se si verificano problemi di permessi di scrittura.

## Vedi anche

- [ClojureDocs - File Handling](https://clojuredocs.org/clojure.core/slurp)
- [ClojureCSV](https://github.com/clojure-csv/clojure-csv)
- [flatland/useful](https://github.com/flatland/useful)