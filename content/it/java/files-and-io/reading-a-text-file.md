---
date: 2024-01-20 17:54:37.860452-07:00
description: "Leggere un file di testo in Java significa estrarre dati salvati in\
  \ un file sul tuo computer. I programmatori lo fanno per manipolare, visualizzare\
  \ o\u2026"
lastmod: '2024-03-13T22:44:43.326434-06:00'
model: gpt-4-1106-preview
summary: Leggere un file di testo in Java significa estrarre dati salvati in un file
  sul tuo computer.
title: Lettura di un file di testo
weight: 22
---

## How to:
Usiamo `Files.readAllLines` per leggere velocemente tutte le linee da un file:

```java
import java.nio.file.Files;
import java.nio.file.Paths;
import java.io.IOException;
import java.util.List;

public class LeggiFile {
    public static void main(String[] args) {
        String percorso = "esempio.txt";
        
        try {
            List<String> righe = Files.readAllLines(Paths.get(percorso));
            righe.forEach(System.out::println);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Output:
``` 
Prima riga del file
Seconda riga del file
Terza riga del file
```

## Deep Dive:
L'API java.nio, introdotta in Java 4 e ampiamente migliorata in Java 7 con NIO.2, è la moderna soluzione per I/O in Java. Prima era comune usare `BufferedReader` in java.io, ma `Files.readAllLines` è più semplice per casi d'uso comuni.

Alternative? `Scanner` per input più complesso, `BufferedReader` per grandi file o streams continui. Dettaglio implementativo? Usa `Charset` quando leggi testo con encoding specifico. `readAllLines` usa l'encoding predefinito se non specificato.

## See Also:
- Documentazione Oracle su `Files.readAllLines`: https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html#readAllLines-java.nio.file.Path-
- Tutorial su `BufferedReader`: https://www.baeldung.com/java-buffered-reader
- Guida allo `Scanner` in Java: https://docs.oracle.com/javase/tutorial/essential/io/scanning.html
