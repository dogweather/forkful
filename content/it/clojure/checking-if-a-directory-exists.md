---
title:                "Verifica dell'esistenza di una directory"
html_title:           "Arduino: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Verificare l'esistenza di una directory per assicurarsi che il percorso sia valido e per evitare errori durante la lettura o la scrittura dei file.

## How to: (Come fare:)
Clojure utilizza la libreria Java per gestire le operazioni sui file. Ecco un esempio di codice:

```Clojure
(import '[java.nio.file Files Paths])

(defn directory-exists? [dir-path]
  (Files/exists (Paths/get dir-path (into-array String []))))

(println (directory-exists? "/path/to/your/directory")) ;; cambia con il percorso reale
```

Risultato di esempio:

```
true ;; se la directory esiste
false ;; se la directory non esiste
```

## Deep Dive (Approfondimento)
Clojure, lavorando sulla JVM (Java Virtual Machine), fornisce interfacce verso le API Java. `java.nio.file.Files` e `java.nio.file.Paths` sono classi introdotte in Java 7, parte del New I/O (NIO).

Prima di Java 7, si usava `java.io.File`, ma NIO offre più flessibilità e prestazioni. `Files/exists` verifica l'esistenza del percorso specificato rispetto a vari `LinkOption`. `Paths/get` crea un oggetto `Path` dal percorso in stringa.

Alternativa: si potrebbe usare `clojure.java.io/file` seguito da `.exists` su un oggetto `File`, ma è meno moderno e manca di alcune features di `java.nio`.

Dettagli implementativi: `Files/exists` effettua un controllo di accesso. Se il file è inaccessibile (per motivi di sicurezza), può scattare un `SecurityException`.

## See Also (Vedi Anche)
- [Clojure Documentation](https://clojure.org/)
- [Java NIO File Operations](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)
- [Clojure `java.io` Compatibility](https://clojure.github.io/clojure/clojure.java.io-api.html)
