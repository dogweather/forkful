---
title:                "Clojure: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

##Perché

Scrivere un file di testo è un elemento fondamentale della programmazione in Clojure. Questo semplice atto permette di salvare, condividere e riprodurre codice in modo efficiente e leggibile. Inoltre, la scrittura di file di testo è utile per l'automazione dei processi e per la creazione di documentazione.

##Come

Per scrivere un file di testo in Clojure è necessario utilizzare la funzione ```spit```. Questa funzione prende due argomenti: il nome del file e il suo contenuto. Ad esempio, per creare un file di testo chiamato "esempio.txt" con il testo "Ciao Mondo!", si può utilizzare il seguente codice:

```Clojure
(spit "esempio.txt" "Ciao Mondo!")
```

Una volta eseguito il codice, il file di testo verrà creato nella stessa cartella in cui è presente il file .clj e conterrà il testo specificato.

È possibile anche scrivere più righe di testo all'interno di un file utilizzando la funzione ```newline``` e concatenando le stringhe con l'operatore ```str```. Ad esempio, il seguente codice creerà un file di testo con tre righe:

```Clojure
(spit "esempio.txt" (str "Ciao \n" "Mondo \n" "in Clojure!""))
```

##Approfondimento

La funzione ```spit``` permette anche di scrivere in file di testo formattati in markdown. Ad esempio, se si vuole creare un file .md con un elenco puntato di tre elementi, si può utilizzare il seguente codice:

```Clojure
(spit "esempio.md" (str "- primo elemento \n" "- secondo elemento \n" "- terzo elemento"))
```

Inoltre, è possibile specificare la modalità di scrittura del file utilizzando l'opzione ```:append```. Questo permetterà di aggiungere del testo a un file già esistente anziché sostituirlo completamente.

##Vedi anche

- [Documentazione ufficiale di Clojure](https://clojure.org/)
- [Tutorial introduttivo a Clojure](https://www.tutorialspoint.com/clojure/)
- [Guida all'utilizzo di Clojure per scrivere file di testo](https://clojuredocs.org/clojure.core/spit)