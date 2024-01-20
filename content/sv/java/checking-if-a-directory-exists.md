---
title:                "Kontrollera om en katalog finns"
html_title:           "Bash: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att kontrollera om en katalog existerar är processen genom vilken din java-programvara undersöker om en viss filkatalog finns på datorn. Detta görs av programmerare för att undvika fel, genom att bekräfta katalogens tillgänglighet innan någon operation utförs på den.

## Hur till:

För att kontrollera om en katalog existerar, kan du använda Java's `java.nio.file` bibliotek som här:

```Java
import java.nio.file.*;

public class CheckDirectory {
    public static void main(String[] args) {
        Path path = Paths.get("din/katalog/sökväg");

        if (Files.exists(path)) {
            System.out.println("Katalogen existerar.");
        } else {
            System.out.println("Katalogen existerar inte.");
        }
    }
}
```
Om katalogen finns, skulle utskriften vara: "Katalogen existerar." Om inte, skulle den säga "Katalogen existerar inte."

## Djupgående dykning

Java's `java.nio.file` bibliotek, inkluderande `Paths` och `Files` klasserna, introducerades i Java 7. Det är en förbättring på gamla `java.io.File` klassen, vilken också kan användas till att kontrollera om en katalog existerar, men är mindre flexibel och kraftfull.

Alternativlösningar finns i form av tredjepartsbibliotek, såsom Apache Commons IO. Men `java.nio.file` anses vara standardmetoden och är dessutom inbyggd och behöver ingen tredjepartsinstallation.

Implementeringsfaktumen är att `Files.exists(path)` metod faktiskt anropar operativsystemet för att bestämma om det angivna sökvägen är en katalog, och dess därför kostsam i termer av prestanda. Om stora mängder av katalogkontroller behövs, överväg att cacha resultaten.

## Se även

- Java 7 File I/O (Paths and Files) ([Oracle docs](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html))
- Apache Commons IO Library ([Apache Commons IO](https://commons.apache.org/proper/commons-io/))