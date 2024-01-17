---
title:                "Scrivere un file di testo"
html_title:           "Clojure: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Scrivere un file di testo significa creare un documento digitale che contiene testo e può essere salvato sul tuo computer o dispositivo. I programmatori spesso scrivono file di testo per memorizzare informazioni o dati, come ad esempio i codici di un programma che stanno sviluppando.

## Come si fa:

**Scrivere un file di testo vuoto:**
```Clojure
(with-open [file (io/writer "test.txt")]
  (.write file ""))
```

**Scrivere del testo in un file:**
```Clojure
(with-open [file (io/writer "test.txt")]
  (.write file "Questo è un testo di esempio"))
```

**Scrivere una lista di dati in un file:**
```Clojure
(with-open [file (io/writer "data.txt")]
  (doseq [num (range 10)]
    (.write file (str num "\n"))))
```

**Output file "data.txt":**
```
0
1
2
3
4
5
6
7
8
9
```

## Approfondimento:

Scrivere file di testo è un processo fondamentale nella programmazione, che risale ai primi linguaggi di programmazione come il COBOL e il FORTRAN. Oltre a creare e salvare file di testo su un dispositivo, i programmatori spesso utilizzano librerie o moduli specifici per manipolare file di testo, come ad esempio per leggere o modificare il contenuto di un file.

## Vedi anche:

- [L'interfaccia "io" di Clojure](https://clojuredocs.org/clojure.core/io)
- [Esempi di utilizzo di file di testo in Clojure](https://www.baeldung.com/clojure-working-with-files)