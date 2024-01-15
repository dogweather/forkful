---
title:                "Skapa en temporär fil"
html_title:           "Java: Skapa en temporär fil"
simple_title:         "Skapa en temporär fil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Varför
*Varför skulle någon vilja skapa en temporär fil i Java?*

Att skapa temporära filer i Java är ett användbart verktyg för att hantera data som endast behövs temporärt under exekveringen av ett program. Det kan vara till nytta när man behöver lagra data temporärt, till exempel för att utföra beräkningar eller bearbetningar.

## Så här gör du
*Här är ett enkelt kodexempel för att skapa en temporär fil i Java:*

```Java
import java.io.File;
import java.io.IOException;

public class TemporaryFile {

    public static void main(String[] args) {
        try {
            File tempFile = File.createTempFile("test", ".txt");
            System.out.println("Temporär fil skapad på: " + tempFile.getAbsolutePath());

            // Vid slutet av programmet, ta bort den temporära filen
            tempFile.deleteOnExit();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
**Output:**

`Temporär fil skapad på: C:\Users\Namn\AppData\Local\Temp\testXXXXXX.txt`

## Djupdykning
Det finns flera användbara metoder för att skapa temporära filer i Java, till exempel `createTempFile()` som vi använde i exemplet ovan. Denna metod skapar en temporär fil på systemets standardplats för temporära filer. 

En annan metod är `createTempFile(String prefix, String suffix)`, där du också kan ange ett prefix och ett suffix för den temporära filen. Prefix-förlängningen läggs till filnamnet före den automatiskt genererade delen (i exemplet ovan var prefixet "test") och suffix-förlängningen läggs till efter.

Det finns också en möjlighet att välja en specifik mapp för den temporära filen att skapas i, genom att använda `createTempFile(String prefix, String suffix, File directory)`.

En annan användbar metod är `File.createTempFile(prefix, suffix, directory)` som skapar en temporär fil direkt i en specifik mapp utan att behöva välja den genom att använda `File`-objektet.

## Se även
- [Java File API](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java I/O Tutorial](https://www.tutorialspoint.com/java/io/java_io_file.htm)
- [How to Create a Temporary File in Java](https://www.baeldung.com/java-temp-file)