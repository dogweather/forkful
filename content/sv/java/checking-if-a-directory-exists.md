---
title:                "Java: Kontrollera om en mapp finns"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en katalog finns kan vara en viktig del i utveckling av Java-program. Det kan hjälpa till att säkerställa att programmet fungerar korrekt och att den förväntade katalogen finns tillgänglig för att lagra eller hämta data.

## Så här gör du

För att kontrollera om en katalog finns i Java, kan du använda metoden `exists()` i klassen `File`. Detta kommer att returnera en boolean (true eller false) som indikerar om katalogen finns eller inte.

```Java
File katalog = new File("minKatalog/");

if (katalog.exists()) { 
  System.out.println("Katalogen finns!");
} else {
  System.out.println("Katalogen finns inte.");
}
```

Om katalogen finns kommer utskriften att vara "Katalogen finns!", annars kommer den att vara "Katalogen finns inte.".

## Djupdykning

När man använder metoden `exists()` för att kontrollera om en katalog finns, är det viktigt att tänka på att den bara returnerar en boolean som indikerar om katalogen finns vid den exakta sökvägen som anges. Det betyder att om du anger en relativ sökväg, till exempel "minKatalog/", kommer metoden att söka efter katalogen i den aktuella arbetskatalogen.

För att kontrollera om en katalog finns oavsett arbetskatalog, kan du använda metoden `getAbsolutePath()`. Denna metod returnerar hela sökvägen till katalogen och möjliggör en mer robust kontroll av katalogens existens.

```Java
File katalog = new File("minKatalog/");
String absolutSökväg = katalog.getAbsolutePath();

if (katalog.exists()) {
  System.out.println("Katalogen finns på " + absolutSökväg);
} else {
  System.out.println("Katalogen finns inte på " + absolutSökväg);
}
```

## Se även

- [Java File-klassens API](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java File-exempelkod](https://www.baeldung.com/java-check-file-directory-exists)
- [Java String-klassens API](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html) (användbar för att hantera sökvägar)