---
title:    "Java: Kontrollera om en mapp existerar"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp finns kan vara en viktig del av en Java-programmerares arbete, särskilt vid hantering av filer och mappar i en applikation. Det kan bidra till att säkerställa att koden fungerar korrekt och undvika eventuella fel.

## Hur man gör det

För att kontrollera om en mapp finns i Java, kan vi använda metoden `exists()` från klassen `File`. Detta är en enkel och pålitlig metod som returnerar `true` eller `false` beroende på om mappen existerar eller inte.

```Java
File directory = new File("C:\\Users\\Username\\Documents\\SampleFolder");
if (directory.exists()) {
    System.out.println("Mappen finns!");
} else {
    System.out.println("Mappen finns inte.");
}
```

Om mappen `SampleFolder` finns i det angivna sökvägen kommer programmet att skriva ut "Mappen finns!", annars kommer det att skriva ut "Mappen finns inte."

## Utforska djupare

Förutom metoden `exists()`, kan vi också använda metoden `isDirectory()` för att kontrollera om en fil är en mapp eller inte. Om en fil är en mapp kommer denna metod att returnera `true`, medan den i annat fall kommer att returnera `false`.

```Java
File file = new File("C:\\Users\\Username\\Documents\\SampleFile.txt");
if (file.isDirectory()) {
    System.out.println("Filen är en mapp.");
} else {
    System.out.println("Filen är inte en mapp.");
}
```

Om `SampleFile.txt` är en mapp kommer programmet att skriva ut "Filen är en mapp.", annars kommer det att skriva ut "Filen är inte en mapp."

## Se även

Här är några användbara länkar för mer information om att kontrollera om en mapp finns i Java:

- [Java File API dokumentation](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java FileExists tutorial](https://docs.oracle.com/javase/tutorial/essential/io/check.html)
- [Articles from Java Code Geeks about checking directory existence](https://www.javacodegeeks.com/?s=check+directory+existence)