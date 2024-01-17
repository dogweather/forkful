---
title:                "Skapa en tillfällig fil"
html_title:           "Java: Skapa en tillfällig fil"
simple_title:         "Skapa en tillfällig fil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skapandet av en temporär fil är en vanlig praxis bland programmerare för att hantera tillfälliga data eller utföra vissa operationer. Det är ett sätt att tillfälligt lagra data som inte behöver sparas permanent eller för att utföra en snabb operation som genererar ett resultat.

## Hur man gör det:
Ett enkelt sätt att skapa en temporär fil i Java är att använda klassen `File` och metoden `createTempFile()` som finns tillgänglig från Java SE 7 och uppåt. Här är ett kodexempel:

```java
// Skapa en temporär fil
File tempFile = File.createTempFile("temp", ".txt");

// Skriv tillfälliga data till filen
FileWriter writer = new FileWriter(tempFile);
writer.write("Det här är en temporär fil.");
writer.close();

// Läs från filen
FileReader reader = new FileReader(tempFile);
int character;
while ((character = reader.read()) != -1) {
    System.out.print((char) character);
}
reader.close();

// Resultat:
// Det här är en temporär fil.
```

För att ta bort en temporär fil efter användning kan du använda metoden `delete()`:

```java
// Ta bort filen
tempFile.delete();
```

## Djupdykning:
Det är inte alltid lämpligt eller säkert att använda en temporär fil, speciellt om filen innehåller känslig data. I så fall kan det vara bättre att använda en temporär databas eller lagra datan i minnet istället för en fil.

Förutom `File`-klassen finns det också andra sätt att skapa en temporär fil, till exempel genom att använda `FileUtils` från Apache Commons IO-biblioteket eller `TemporaryFolder` från JUnit-testramen.

När man skapar en temporär fil i Java används operativsystemets standardmapp för temporära filer. Om du vill ange en annan mapp kan du använda metoden `createTempFile(String prefix, String suffix, File directory)` och ange önskad mapp som sista argument.

## Se även:
Läs mer om klassen `File` och dess metoder här: https://docs.oracle.com/javase/8/docs/api/java/io/File.html

Mer info om undvikande av temporära filer i Java: https://blog.codinghorror.com/zipped-my-doodads/

Läs mer om Apache Commons IO här: https://commons.apache.org/proper/commons-io/

Läs mer om JUnit här: https://junit.org/junit5/