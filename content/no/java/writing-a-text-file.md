---
title:                "Skriving av en tekstfil"
date:                  2024-01-19
html_title:           "Arduino: Skriving av en tekstfil"
simple_title:         "Skriving av en tekstfil"

category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Å skrive til en tekstfil er en prosess hvor du lagrer data som tekst i en fil. Programmerere gjør dette for å lagre konfigurasjoner, eksportere rapporter, logge feil, eller lage varige data.

## How to:
Java tilbyr `FileWriter` for enkel tekstskriving. Her er hvordan:

```java
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriter {
    public static void main(String[] args) {
        String data = "Hei! Dette er en tekstfil.";

        try (FileWriter writer = new FileWriter("eksempel.txt")) {
            writer.write(data);
            System.out.println("Filen er skrevet.");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
Output:
```
Filen er skrevet.
```

## Deep Dive
Før `FileWriter` og `BufferedWriter`, brukte vi strømmer som `FileOutputStream`. Alternativer i moderne Java inkluderer `Files`-klassen med dens `write()`-metode som tilbyr mer kontroll og effektivitet. Skriving kan implementeres synkront eller asynkront basert på behov.

## See Also
- [Oracle Java Dokumentasjon – FileWriter](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/io/FileWriter.html)
- [Oracle Java Dokumentasjon – Files.write](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html#write(java.nio.file.Path,java.lang.Iterable,java.nio.charset.Charset,java.nio.file.OpenOption...))
- [Baeldung – Skrive til filer i Java](https://www.baeldung.com/java-write-to-file)
