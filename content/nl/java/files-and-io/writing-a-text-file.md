---
title:                "Een tekstbestand schrijven"
aliases:
- /nl/java/writing-a-text-file.md
date:                  2024-01-28T22:12:38.531219-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tekstbestand schrijven"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/java/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een tekstbestand schrijven in Java betekent tekstgegevens opslaan op een bestand op schijf. Ontwikkelaars doen dit voor taken zoals loggen, configureren, of het exporteren van menselijk leesbare gegevens.

## Hoe te:

Met Java's `java.nio.file` pakket is schrijven naar een tekstbestand eenvoudig. Bekijk `Files.write()` voor een snelle opslag:

```java
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class WriteTextFileExample {
    public static void main(String[] args) {
        List<String> lijnen = List.of("Regel 1", "Regel 2", "Regel 3");
        Path bestand = Path.of("voorbeeld.txt");

        try {
            Files.write(bestand, lijnen);
            System.out.println("Succesvol geschreven naar het bestand.");
        } catch (IOException e) {
            System.err.println("Oeps! Er is een fout opgetreden: " + e.getMessage());
        }
    }
}
```

Output:
```
Succesvol geschreven naar het bestand.
```

## Diepgaande Duik

In de goede oude tijd ging Java I/O helemaal over `FileWriter` en `BufferedWriter`. Nu is het NIO-pakket (`java.nio.file`) de te volgen weg. `Files.write()` is handig—het handelt creatie, opening, en schrijven in één keer af. Alternatief? `FileOutputStream` voor controle op byte-niveau. Onder de motorkap gebruikt `Files.write()` een `BufferedWriter` en `Charset` om tekst als bytes te coderen.

## Zie Ook

Duik dieper in bestands-I/O met deze links:

- Officiële documentatie van `java.nio.file.Files`: https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html
- Oracle's gids over bestands-I/O: https://docs.oracle.com/javase/tutorial/essential/io/
- Voor een byte-georiënteerde aanpak, verken `FileOutputStream`: https://docs.oracle.com/javase/8/docs/api/java/io/FileOutputStream.html
