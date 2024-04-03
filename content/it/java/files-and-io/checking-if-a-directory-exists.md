---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:33.771904-07:00
description: "Verificare se una directory esiste in Java \xE8 un'operazione fondamentale\
  \ che comporta la verifica della presenza di una directory nel file system prima\
  \ di\u2026"
lastmod: '2024-03-13T22:44:43.323294-06:00'
model: gpt-4-0125-preview
summary: "Verificare se una directory esiste in Java \xE8 un'operazione fondamentale\
  \ che comporta la verifica della presenza di una directory nel file system prima\
  \ di leggerla, scriverci o eseguire qualsiasi operazione che richieda la sua esistenza."
title: Verifica se una directory esiste
weight: 20
---

## Come fare:
In Java, ci sono diversi modi per verificare se una directory esiste, principalmente utilizzando le classi `java.nio.file.Files` e `java.io.File`.

**Usando `java.nio.file.Files`**:

Questo è l'approccio consigliato nelle versioni più recenti di Java.

```java
import java.nio.file.Files;
import java.nio.file.Paths;

public class DirectoryExists {
    public static void main(String[] args) {
        // Specifica qui il percorso della directory
        String directoryPath = "path/to/directory";

        // Verifica se la directory esiste
        if (Files.exists(Paths.get(directoryPath))) {
            System.out.println("La directory esiste.");
        } else {
            System.out.println("La directory non esiste.");
        }
    }
}
```
**Output di esempio**:
```
La directory esiste.
```
Oppure 
```
La directory non esiste.
```

**Usando `java.io.File`**:

Anche se si raccomanda di utilizzare `java.nio.file.Files`, si può anche usare la classe più vecchia `java.io.File`.

```java
import java.io.File;

public class DirectoryExistsLegacy {
    public static void main(String[] args) {
        // Specifica qui il percorso della directory
        String directoryPath = "path/to/directory";

        // Creazione di un oggetto File
        File directory = new File(directoryPath);

        // Verifica se la directory esiste
        if (directory.exists() && directory.isDirectory()) {
            System.out.println("La directory esiste.");
        } else {
            System.out.println("La directory non esiste.");
        }
    }
}
```
**Output di esempio**:
```
La directory esiste.
```
Oppure
```
La directory non esiste.
```

**Usando Librerie di Terze Parti**:

Anche se la libreria standard di Java di solito è sufficiente per questo compito, librerie di terze parti come Apache Commons IO offrono ulteriori utilità di gestione dei file che potrebbero essere utili in applicazioni più complesse.

**Apache Commons IO**:

Prima, aggiungi la dipendenza di Apache Commons IO al tuo progetto. Poi, puoi utilizzare le sue funzionalità per verificare l'esistenza di una directory.

```java
// Assumendo che Apache Commons IO sia aggiunto al progetto

import org.apache.commons.io.FileUtils;

public class DirectoryExistsCommons {
    public static void main(String[] args) {
        // Specifica qui il percorso della directory
        String directoryPath = "path/to/directory";

        // Utilizzo di FileUtils per verificare
        boolean directoryExists = FileUtils.directoryContains(new File(directoryPath), null);

        if (directoryExists) {
            System.out.println("La directory esiste.");
        } else {
            System.out.println("La directory non esiste.");
        }
    }
}
```

**Nota**: `FileUtils.directoryContains` verifica se una directory contiene un file specifico, ma passando `null` come secondo argomento, è possibile utilizzarlo per controllare l'esistenza della directory. Fai attenzione, poiché questo potrebbe non essere l'uso più diretto o intenzionale del metodo.

**Output di esempio**:
```
La directory esiste.
```
Oppure
```
La directory non esiste.
```
