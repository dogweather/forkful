---
title:                "Java: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför
I Java är det viktigt att kunna kolla om en mapp finns eller inte. Det är användbart när du behöver hantera filer och göra säkerhetskontroller i ditt program.

## Hur man gör
För att kolla om en mapp finns, används metoden `exists()` från klassen `File` i Java. Här är ett exempel på kod:

```Java
File directory = new File("minMapp");

// Kontrollera om mappen finns
if(directory.exists()){
  System.out.println("Mappen finns.");
} else {
  System.out.println("Mappen finns inte.");
}
```

I detta exempel skapar vi en ny `File`-objekt och kallar den "minMapp". Sedan använder vi `exists()`-metoden för att kontrollera om denna mapp faktiskt finns. Om den gör det, kommer vi att få utskriften "Mappen finns.", annars kommer vi att få utskriften "Mappen finns inte.".

## Djupdykning
Vi kan också utföra andra åtgärder baserat på resultatet av `exists()`-metoden. Till exempel, om mappen inte finns, kan vi använda `mkdir()`-metoden för att skapa den. Vi kan också använda `isDirectory()`-metoden för att säkerställa att det är en mapp och inte en fil. Det finns också andra metoder som `isHidden()` och `canRead()` som kan vara användbara i olika scenarier.

Att kolla om en mapp finns är också användbart i samband med try-with-resources-satsen, då vi kan undvika att försöka stänga en mapp som inte finns.

## Se även
- [Java File-klassens dokumentation](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java IO-tutorial på svenska](https://www.javatpoint.com/java-io-tutorial)
- [Enkel guide till fil- och mapphantering i Java](https://www.baeldung.com/java-io)