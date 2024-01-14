---
title:                "Clojure: Verifica se una cartella esiste"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Spesso mentre programmiamo, ci troviamo di fronte alla necessità di verificare l'esistenza di una directory. Questa operazione può essere utile per evitare errori o per gestire dinamicamente le risorse del sistema. In questo post, esploreremo come verificare se una directory esiste utilizzando Clojure.

## Come fare

Prima di tutto, dobbiamo importare la libreria `clojure.java.io` per avere accesso alle funzioni di I/O. Successivamente, possiamo utilizzare la funzione `file?` per verificare se un determinato percorso corrisponde a una directory. Vediamo un esempio pratico:

```Clojure
(ns my-namespace
  (:require [clojure.java.io :as io]))
  
(io/file? "/percorso/directory") ; => false
(io/file? "/percorso/file.txt") ; => true
```

Come mostrato nell'esempio, la funzione `file?` restituisce `true` se il percorso specificato corrisponde a un file e `false` se corrisponde a una directory. È anche possibile utilizzare la funzione `exists?` per verificare l'esistenza di qualsiasi tipo di percorso (file o directory).

```Clojure
(io/exists? "/percorso/directory") ; => true
(io/exists? "/percorso/inventato") ; => false
```

Inoltre, se vogliamo controllare se una directory è effettivamente scrivibile, possiamo utilizzare la funzione `writable?`.

```Clojure
(io/writable? "/percorso/directory") ; => true
(io/writable? "/percorso/file.txt") ; => false
```

## Approfondimento

La funzione `file?` utilizza il metodo `isDirectory` della classe `File` del linguaggio Java per determinare se il percorso corrisponde a una directory. Questo significa che è possibile utilizzare qualsiasi percorso supportato dalla classe `File` come argomento per questa funzione. Inoltre, è possibile specificare percorso assoluto o relativo, a patto che il percorso sia valido.

## Vedi anche

- Documentazione ufficiale di `clojure.java.io`: https://clojure.github.io/clojure/clojure.java.io-api.html
- Tutorial su operazioni di I/O in Clojure: https://www.baeldung.com/clojure-io