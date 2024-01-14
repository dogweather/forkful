---
title:                "Java: Skapa en tillfällig fil"
simple_title:         "Skapa en tillfällig fil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Att skapa temporära filer är en vanlig uppgift för Java-programmerare att lösa som kan bidra till att effektivisera arbetsprocessen och förbättra programmet. Genom att skapa en temporär fil kan man tillfälligt lagra data som behöver användas senare i programmet, och på så sätt undvika att hantera större mängder data i minnet.

## Hur man skapar en temporär fil

Att skapa en temporär fil i Java är en relativt enkel process. Först behöver man importera klassen `java.io.File` som innehåller metoder för att hantera filer. Sedan kan man använda konstruktören `createTempFile()` som tar in två parametrar - ett prefix för filnamnet och ett suffix för filtypen. Exempelvis:

```Java
File tempFile = File.createTempFile("temp", ".txt");
```

I detta exempel kommer en temporär fil att skapas med namnet "temp" och filtypen "txt". Man kan även specificera en mapp där filen ska skapas genom att lägga till en tredje parameter för `createTempFile()`. När en temporär fil har skapats, kan man använda den precis som en vanlig fil i sitt program.

## Introduktion till temporära filer

Att skapa en temporär fil innebär också en del bakomliggande processer som kan vara nyttiga att känna till. När en temporär fil skapas, genereras den på det operativsystems-specifika temporära området (ofta kallat "tmp"). När programmet avslutas kommer denna fil automatiskt att raderas. Om man vill behålla filen längre kan man dock använda metoden `deleteOnExit()` tillsammans med sin temporära fil. Detta kommer att säkerställa att filen raderas när programmet avslutas oavsett anledning.

## See Also
- [Java File API Docs](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Working with temporary files in Java](https://www.baeldung.com/java-temporary-files)
- [Understanding temporary files in Java](https://medium.com/jacktan/understanding-temporary-files-in-java-4c2bad71b896)