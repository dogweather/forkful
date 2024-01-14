---
title:                "Java: Läsning av en textfil"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa och bearbeta textfiler är en vanlig uppgift inom programmering, oavsett om du är nybörjare eller erfaren. Att kunna läsa in data från en textfil kan hjälpa dig att automatisera processer och göra dina program mer intelligent. Läs vidare för att lära dig hur du kan läsa en textfil i Java.

## Så här

Det första steget för att läsa en textfil i Java är att skapa en instans av klassen `File` med hjälp av filnamnet som argument. Detta kommer att representera den faktiskt filen på ditt filsystem.

```Java
File file = new File("input.txt"); // skapa en instans av File-klassen med filnamnet input.txt
```

Nästa steg är att skapa en instans av klassen `Scanner` som kommer att hjälpa dig att läsa in data från filen. Detta görs genom att skicka in din `File`-instans som argument till `Scanner`-konstruktorn.

```Java
Scanner scanner = new Scanner(file); // skapa en instans av Scanner-klassen med File-instanse som argument
```

Nu är det dags att börja läsa filen. Du kan använda metoden `nextLine()` för att läsa in en rad åt gången och `hasNextLine()` för att kontrollera om det finns fler rader att läsa.

```Java
while (scanner.hasNextLine()) {
    String line = scanner.nextLine(); // läs in en rad och spara den i en variabel

    System.out.println(line); // skriv ut raden till konsolen
}
```

I exemplet ovan kommer varje rad i filen att skrivas ut till konsolen. Nu är det upp till dig att bearbeta datan på ett sätt som passar ditt program.

## Djupdykning

När det gäller bearbetning av en textfil, finns det många andra saker du kan göra med hjälp av `Scanner`-klassen. Till exempel kan du använda metoder som `nextInt()` och `nextDouble()` för att läsa in numeriska värden från filen. Du kan också använda `useDelimiter()` för att specificera ett tecken eller en sträng som ska användas som avskiljare mellan värden.

Du kan också skriva till en textfil på liknande sätt genom att använda klassen `FileWriter` och dess metoder som `write()` och `append()`. Kom bara ihåg att stänga filen när du är klar genom att använda metoden `close()`.

## Se även

- Java: Läs och skriv till textfiler [https://www.w3schools.com/java/java_files_read.asp]
- Scanner-klassens Java API [https://docs.oracle.com/javase/7/docs/api/java/util/Scanner.html]
- FileWriter-klassens Java API [https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html]