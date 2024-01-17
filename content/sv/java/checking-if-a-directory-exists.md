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

## Vad & Varför?
Att kontrollera om en mapp existerar är en viktig del av programmering eftersom det låter oss kolla om en specifik mapp finns i vårt system eller inte. Detta kan vara avgörande i många fall, som att säkerställa att vår kod fungerar korrekt eller att undvika att skriva över befintliga filer.

## Hur gör man:
För att kontrollera om en mapp existerar i Java, kan vi använda oss av metoden `exist()` i klassen `java.io.File`. Vi skapar ett `File` objekt för vår mapp och sedan använder vi metoden `exist()` för att kolla om den existerar. Om mappen existerar, returnerar metoden `true`, annars returnerar den `false`.

```Java
import java.io.File;

public class CheckDirectory {
    public static void main(String[] args) {
        File myDirectory = new File("C:/Users/UserName/Desktop/myFolder");
        if(myDirectory.exist()) {
            System.out.println("Mappen existerar.");
        } else {
            System.out.println("Mappen existerar inte.");
        }
    }
}
```

Output:

```
Mappen existerar inte.
```

## Djupdykning:
Kontrollen ifall en mapp existerar är en viktig del av filsystemshantering och har funnits sedan de tidiga dagarna av Java. Tidigare användes metoden `isDirectory()` i klassen `java.io.File` för att kontrollera om en mapp existerar, men den anses nu som föråldrad och ersätts av metoden `exist()`. Det finns också andra alternativ för att kontrollera mappens existens, som att använda sig av `java.nio.file` paketet eller att använda sig av specialbibliotek för filsystemshantering.

## Se även:
- [Java Dokumentation för klassen `java.io.File`](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Java Dokumentation för paketet `java.nio.file`](https://docs.oracle.com/javase/7/docs/api/java/nio/file/package-summary.html)
- [Apache Commons IO - ett populärt bibliotek för filsystemshantering](https://commons.apache.org/proper/commons-io/)