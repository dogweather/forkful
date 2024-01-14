---
title:                "Clojure: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo può essere utile durante lo sviluppo di un programma, per esempio per memorizzare dati temporanei o per creare un file di log. In questa guida impareremo come creare e gestire i file temporanei in Clojure.

## Come Fare

La creazione di un file temporaneo in Clojure è semplice e si può fare in pochi passaggi.

```Clojure
(import java.io.File)
(import java.nio.file.Files)

;; Definiamo il percorso e il nome del file temporaneo
(def temp-file (str "temp/" "file_temporaneo.txt"))

;; Creiamo il file usando la funzione createTempFile del package java.nio.file
(Files/createTempFile (File. temp-file) nil)

;; Otterremo il seguente output
#object[java.io.File 0x5283486f temp/file_temporaneo.txt]

```

Ora il nostro file temporaneo è stato creato con successo e possiamo utilizzarlo nel nostro programma.

È importante notare che il file temporaneo verrà automaticamente eliminato quando il programma termina. Se si desidera eliminare il file prima della fine del programma, si può usare la funzione delete del package java.io.

```Clojure
;; Eliminiamo il file temporaneo
(.delete (File. temp-file))
```

## Deep Dive

La creazione di un file temporaneo in Clojure si basa sul package java.nio.file, in particolare sulla funzione createTempFile. Questa funzione crea un file temporaneo nel percorso specificato e restituisce un oggetto di tipo java.io.File. È possibile specificare anche un prefisso e un suffisso per il nome del file, come ad esempio "temp_" e ".txt".

Inoltre, è possibile specificare una directory specifica per creare il file temporaneo, invece di utilizzare la directory di default che viene restituita dalla funzione System/getProperty.

## Vedi Anche

- [java.nio.file API](https://docs.oracle.com/javase/8/docs/api/java/nio/file/package-summary.html)
- [Java SE Documentation](https://docs.oracle.com/en/java/javase/index.html)