---
title:                "Skapa en temporär fil"
date:                  2024-01-20T17:40:41.462216-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skapa en temporär fil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skapa en temporär fil innebär att förse ett program med en fil som bara existerar under dess körtid, och som oftast raderas automatiskt. Programmerare gör det för att hantera data temporärt utan att riskera användarens data eller kladda ner filsystemet.

## Hur man gör:
```java
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class TempFileExample {
    public static void main(String[] args) {
        try {
            // Skapar en temporär fil
            Path tempFile = Files.createTempFile("tempfile_", ".tmp");
            
            System.out.println("Temporär fil skapad: " + tempFile);
            
            // Använder filen för någon operation...
            // ...

            // Filtömningskod (valfri, eftersom det är en temp fil)
            tempFile.toFile().deleteOnExit();
        } catch (IOException e) {
            System.err.println("Problem med att skapa tempfil: " + e.getMessage());
        }
    }
}
```
Output:
```
Temporär fil skapad: /tmp/tempfile_1234567890.tmp
```

## Djupdykning
Temporära filer har funnits länge och var ett tidigt sätt att undvika att använda för mycket huvudminne. I Java kan `java.io`- och `java.nio`-paketet användas för att skapa temporära filer.

Alternativ till inbyggda Java-metoder inkluderar att använda tredjepartsbibliotek eller skapa egna lösningar för filhantering. Men `Files.createTempFile` är smidigt då det automatiskt väljer rätt katalog för tempfiler enligt operativsystemet och hanterar namngivning för att undvika konflikter.

När det gäller implementeringen, gör `deleteOnExit()`-metoden att JVM:en raderar filen när programmet avslutas. Det är bra i utveckling, men kan vara riskabelt för större applikationer då listan över filer att radera kan bli stor och påverka prestandan.

## Se även
- [The Java™ Tutorials – Reading, Writing, and Creating Files](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Javadoc for java.nio.file.Files](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html)
- [Javadoc for java.io.File](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)