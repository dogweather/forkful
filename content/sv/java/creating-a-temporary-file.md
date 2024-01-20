---
title:                "Att skapa en tillfällig fil"
html_title:           "Bash: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Skapa Temporära Filer i Java

## Vad & Varför?
Att skapa en temporär fil innebär att erbjuda en kortvarig lagringsplats för data i ett javaprogram. Detta görs för att undvika överbelastning av programmet genom att tillfälligt lagra stora data som behövs under programmets körning.

## Hur man gör:
```Java
import java.io.File;
import java.io.IOException;

public class Main {
    public static void main(String[] args) {
        try {
            File temp = File.createTempFile("temp-file-name", ".tmp"); 
            System.out.println("Temp file : " + temp.getAbsolutePath());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
Detta kodstycke skapar en temp-fil med namnet 'temp-file-name' med '.tmp' som prefix. Den fullständiga sökvägen till temp-filen kommer att skrivas ut i konsolen.

## Djup dykning
### Historisk kontext
Java tillhandahåller metoden `createTempFile` i `java.io.File` klassen sedan Java 2. 

### Alternativ
Du kan också använda `Files.createTempFile` metoden som tillhandahålls i `java.nio.file.Files` klassen från Java 7 och framåt.

### Implementeringsdetaljer
När `createTempFile` metoden anropas, skapas temp-filen i den mapp som java systemegenskapen `java.io.tmpdir` pekar på.

## Se även
För mer information om att skapa temporära filer i Java, se:
1. [Oracle's guide to Handling Files](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
2. [Java.IO.File.createTempFile() Method](https://www.tutorialspoint.com/java/io/file_createtempfile_directory.htm)
3. [Java 7 Files.createTempFile() Method](https://www.javacodex.com/Files/Java-Files-createTempFile-Method)