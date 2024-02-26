---
date: 2024-01-20 17:54:28.131315-07:00
description: "\xC5 lese en tekstfil betyr at vi programmerere henter tekstdata fra\
  \ en fil p\xE5 disken. Vi gj\xF8r det for \xE5 manipulere data, hente innstillinger,\
  \ eller\u2026"
lastmod: '2024-02-25T18:49:38.863527-07:00'
model: gpt-4-1106-preview
summary: "\xC5 lese en tekstfil betyr at vi programmerere henter tekstdata fra en\
  \ fil p\xE5 disken. Vi gj\xF8r det for \xE5 manipulere data, hente innstillinger,\
  \ eller\u2026"
title: Lese en tekstfil
---

{{< edit_this_page >}}

## What & Why?
Å lese en tekstfil betyr at vi programmerere henter tekstdata fra en fil på disken. Vi gjør det for å manipulere data, hente innstillinger, eller prosessere informasjon som er lagret i en organisert form.

## How to:
La oss lage et eksempel på hvordan man leser en fil i Java, ved hjelp av `Files.readAllLines`.

```java
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class FileLeseEksempel {
    
    public static void main(String[] args) {
        Path filePath = Path.of("eksempel.txt");
        
        try {
            List<String> lines = Files.readAllLines(filePath);
            
            for (String line : lines) {
                System.out.println(line);
            }
            
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Eksempelutdata når 'eksempel.txt' inneholder noen linjer med tekst:
```
Hei, dette er en test.
Andre linje her.
Og den tredje linjen.
```

## Deep Dive:
Å lese filer i Java har utviklet seg. Vi begynte med `FileInputStream`, men den var tungvint og lavnivå. Så kom `BufferedReader` og `FileReader`, som var enklere. Nå for tiden bruker vi gjerne `Files`-klassen fra `java.nio` pakken fordi den er mer effektiv og gir enklere kode.

Alternativer til `Files.readAllLines` inkluderer å bruke `Scanner` for å lese filer linje for linje eller `BufferedReader` for bedre kontroll og ytelse ved store filer. Man kan også bruke `FileChannel` hvis man trenger høy ytelse ved å jobbe direkte mot filsystem-bufferne.

Når vi leser filer, må vi håndtere `IOException` for å ta høyde for at filen kanskje ikke finnes eller det er skrivebeskyttelse som forhindrer lesing.

## See Also:
Her er noen nyttige lenker for videre lesning:
- [Files.readAllLines Javadoc](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html#readAllLines(java.nio.file.Path))
- [BufferedReader Javadoc](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/io/BufferedReader.html)
- [Oracle's guide to reading, writing, and creating files](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)
