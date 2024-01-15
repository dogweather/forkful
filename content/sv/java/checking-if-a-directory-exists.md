---
title:                "Kontrollera om en mapp finns"
html_title:           "Java: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför
Att kontrollera om en mapp existerar är en viktig del av programmering eftersom det möjliggör för oss att hantera filer och mappar korrekt. Det kan hjälpa oss att undvika felmeddelanden och garantera att vårt program fungerar som det ska.

## Hur man gör
För att kontrollera om en mapp existerar i Java, kan du använda metoden `exists()` från klassen `File`. Här är ett exempel:

```java
import java.io.File;

public class DirectoryCheck {

    public static void main(String[] args) {

        // Använd sökvägen till din mapp här
        File directory = new File("/Users/johndoe/Documents/test");

        if (directory.exists()) {
            System.out.println("Mappen finns!");
        } else {
            System.out.println("Mappen finns inte.");
        }
    }
}
```

Output:
```
Mappen finns inte.
```

Om mappen existerar kommer `exists()` att returnera `true` annars kommer det att returnera `false`.

## Djupdykning
För att förstå hur `exists()` fungerar behöver vi titta på dess implementation. Enligt Java-dokumentationen är implementationen av `exists()` som följer:

```java
public boolean exists() {
    return ((this.status & EXISTS) != 0);
}
```

Det här ser kanske komplicerat ut, men i grund och botten kollar `exists()` om attributet `EXISTS` är satt till `true` för det givna objektet. Detta attribut sätts till `true` när `File`-objektet skapas, eller när `refresh()`-metoden anropas.

## Se också
Här är några andra användbara resurser för att lära sig mer om att arbeta med filer och mappar i Java:

- [Java File-klassens dokumentation](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Creating a Directory in Java](https://www.baeldung.com/java-create-directory)
- [Handling Exceptions in Java](https://www.tutorialspoint.com/java/java_exceptions.htm)

Tack för att du läste! Fortsätt utforska Java och ha kul med programmering! 🚀