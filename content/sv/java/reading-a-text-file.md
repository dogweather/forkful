---
title:    "Java: Läsning av en textfil"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Varför läsa en textfil?

Inom programmering, är att läsa in data från en textfil ett viktigt koncept. Detta öppnar upp möjligheten att behandla och manipulera information från externa källor. Det kan också hjälpa till att organisera stora mängder data för enkel åtkomst och användning. Om du är nybörjare inom Java-programmering, är läsning av textfiler ett grundläggande verktyg som kan hjälpa dig att utveckla dina färdigheter.

## Så här läser du en textfil i Java

För att läsa en textfil i Java, behöver du först skapa en File-objekt. Detta görs genom att ange sökvägen till textfilen som en parameter i konstruktorn för File-klassen. Till exempel, om din textfil ligger i samma mapp som din Java-fil, kan du skapa din File-objekt på följande sätt:

```Java
File file = new File("min_textfil.txt");
```

Nästa steg är att skapa en Scanner-objekt och använda din File-objekt som parameter. Scanner används för att läsa informationen från filen och lagra den i variabler för vidare användning. För att läsa innehållet i din textfil, kan du använda följande kod:

```Java
Scanner scanner = new Scanner(file);
while (scanner.hasNextLine()) {
    String data = scanner.nextLine();
    System.out.println(data);
}
```

Ovanstående kod kommer att skriva ut varje rad av textfilen till konsolen. Du kan också använda olika metoder i Scanner-klassen för att läsa specifika datatyper, som int eller double.

## Djupdykning i läsning av textfiler

I vår kodexempel, använde vi metoden `hasNextLine()` för att kontrollera om det finns en ytterligare rad att läsa i filen. Detta kan vara användbart om du vill läsa filen rad för rad. Om du istället vill läsa hela filen som en sträng, kan du använda metoden `hasNext()` och `next()`.

Du kan också använda en try-with-resources-sats för att automatiskt stänga din Scanner när du är klar med filen, vilket är en god praxis. Detta gör du genom att lägga till `try` och `catch` runt din kod och inom parentesen för try, skriva `Scanner scanner = new Scanner(file)`. När try-blocket är klart, kommer scanner automatiskt att stängas.

## Se även

- Java Dokumentation för Scanner: https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html
- Att skriva till en textfil i Java: LINK
- Att arbeta med filer och mappar i Java: LINK