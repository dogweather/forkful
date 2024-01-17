---
title:                "Leggere un file di testo"
html_title:           "Clojure: Leggere un file di testo"
simple_title:         "Leggere un file di testo"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Cosa & Perchè?

Lettura di un file di testo è il processo di estrarre il contenuto di un file di testo, che può contenere una varietà di informazioni, come testo, numeri, o immagini. I programmatori spesso usano la lettura di un file di testo per estrarre dati da un file e utilizzarlo nel loro programma.

## Come fare:

```Clojure
(with-open [file (clojure.java.io/reader "file.txt")]
  (doall (line-seq file)))
```

Output:
```
("Prima riga del file" "Seconda riga del file" "Terza riga del file")
```

## Approfondimento:

La lettura di un file di testo è un processo comune nella programmazione e ha origini nel mondo dell'informatica. Alcune alternative alla lettura di un file di testo includono utilizzare un database o un formato di dati strutturato come JSON o XML. L'implementazione del codice per la lettura di un file di testo può variare a seconda del linguaggio di programmazione utilizzato.

## Vedi anche:

- [Documentazione ufficiale di Clojure](https://clojure.org/reference/io)
- [Esempio di lettura di un file di testo in Clojure](https://www.guru99.com/clojure-file-handles-read-write.html)
- [Altro approfondimento sulla lettura di file di testo in diversi linguaggi di programmazione](https://www.w3schools.in/c-programming/read-file/)