---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:14.977883-07:00
description: "Come fare: Il pacchetto New I/O (NIO) di Java (`java.nio.file`) offre\
  \ un approccio pi\xF9 versatile per la gestione dei file. Ecco un modo semplicistico\
  \ per\u2026"
lastmod: '2024-03-13T22:44:43.327347-06:00'
model: gpt-4-0125-preview
summary: "Il pacchetto New I/O (NIO) di Java (`java.nio.file`) offre un approccio\
  \ pi\xF9 versatile per la gestione dei file."
title: Scrivere un file di testo
weight: 24
---

## Come fare:


### Utilizzando `java.nio.file` (Libreria Standard)
Il pacchetto New I/O (NIO) di Java (`java.nio.file`) offre un approccio più versatile per la gestione dei file. Ecco un modo semplicistico per scrivere su un file utilizzando `Files.write()`:

```java
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class ScrittoreFileTestoNIO {
    public static void main(String[] args) {
        List<String> righe = Arrays.asList("Riga 1", "Riga 2", "Riga 3");
        try {
            Files.write(Paths.get("esempio.txt"), righe);
            System.out.println("File scritto con successo!");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Output:

```
File scritto con successo!
```

### Utilizzando `java.io` (Libreria Standard)
Per un approccio più tradizionale, `java.io.FileWriter` è una buona scelta per scrivere file di testo semplicemente:

```java
import java.io.FileWriter;
import java.io.IOException;

public class ScrittoreFileTestoIO {
    public static void main(String[] args) {
        try (FileWriter writer = new FileWriter("esempio.txt")) {
            writer.write("Ciao, Mondo!\n");
            writer.append("Questa è un'altra riga.");
            System.out.println("File scritto con successo!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Output:

```
File scritto con successo!
```

### Utilizzando Apache Commons IO
La libreria Apache Commons IO semplifica molte operazioni, inclusa la scrittura di file. Ecco come scrivere su un file utilizzando `FileUtils.writeStringToFile()`:

Prima, aggiungi la dipendenza al tuo progetto. Se usi Maven, includi:

```xml
<dependency>
  <groupId>org.apache.commons</groupId>
  <artifactId>commons-io</artifactId>
  <version>2.11.0</version> <!-- Controlla per l'ultima versione -->
</dependency>
```

Poi, usa il seguente codice per scrivere testo in un file:

```java
import org.apache.commons.io.FileUtils;
import java.io.File;
import java.io.IOException;

public class ScrittoreFileTestoCommonsIO {
    public static void main(String[] args) {
        try {
            FileUtils.writeStringToFile(new File("esempio.txt"), "Questo è un testo scritto utilizzando Commons IO.", "UTF-8");
            System.out.println("File scritto con successo!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

```

Output:

```
File scritto con successo!
```
