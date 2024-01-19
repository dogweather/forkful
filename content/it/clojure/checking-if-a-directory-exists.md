---
title:                "Verificare se una directory esiste"
html_title:           "Clojure: Verificare se una directory esiste"
simple_title:         "Verificare se una directory esiste"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
Il controllo dell'esistenza di una directory è l'atto di verificare se una specifica cartella esiste nel percorso del file system specificato. Lo facciamo per evitare errori durante l'esecuzione quando si cerca di accedere alla directory per leggere o scrivere dati.

## Come fare:

Per controllare se una directory esiste in Clojure, utilizziamo la funzione `clojure.java.io/file` per creare un oggetto File, quindi controlliamo se esiste con `.exists` e se è una directory con `.isDirectory`.

```Clojure
(let [dir (clojure.java.io/file "/path/to/directory")]
  (and (.exists dir) (.isDirectory dir)))
```
Se la directory esiste, il codice restituirà `true`. Altrimenti, restituirà `false`.

## Approfondimento

Controllare l'esistenza di una directory è un concetto fondamentale nel I/O di programmazione. Nel contesto storico, questo è un principio che risale ai primi giorni della programmazione dove l'accesso al disco era un'operazione costosa.

Un'alternativa sarebbe gestire l'eccezione che si verifica quando tentiamo di accedere a una directory inesistente. Ma preverificare con `.exists` e `.isDirectory` può aiutare a evitare inutili eccezioni costose.

L'implementazione su come Clojure controlla l'esistenza di una directory è di fatto un wrap attorno al metodo Java. Il package `clojure.java.io` fornisce funzioni per interagire con il file system, che sfrutta internamente le funzioni dell'API Java File I/O.

## Vedi anche

Se sei interessato a esplorare ulteriormente, ecco alcuni link correlati:
- Documentazione ufficiale di Clojure: [clojure.java.io](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/file)
- Java File I/O API: [Java.io.File](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- Articolo StackOverflow: [How to check if a folder exists? (in clojure)](https://stackoverflow.com/questions/34668321/how-to-check-if-a-folder-exists-in-clojure)