---
title:                "Java: Att läsa en textfil"
simple_title:         "Att läsa en textfil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil är en vanlig uppgift inom programmering. Genom att lära dig hur man läser en textfil kan du enkelt manipulera data och utföra olika åtgärder baserat på dess innehåll. Det är också ett viktigt koncept inom filhantering och kan hjälpa dig att förbättra din kodningsteknik.

## Hur man gör

Först och främst måste vi deklarera en `File` objekt och ange sökvägen till vår textfil. Detta görs genom att skriva:

```Java
File myFile = new File("min_textfil.txt");
```

Efter att vi har deklarerat vår fil, måste vi skapa en `FileReader` objekt. Detta objekt kommer att hantera läsningen av filen. Vi kan göra detta genom att skriva:

```Java
FileReader fileReader = new FileReader(myFile);
```

Nu kan vi använda vår `FileReader` för att skapa en `BufferedReader` som kommer att läsa in vår textfil:

```Java
BufferedReader reader = new BufferedReader(fileReader);
```

När vi har vår `BufferedReader` objekt, kan vi börja läsa vår fil. Detta görs med hjälp av `readLine()` metoden:

```Java
String line = reader.readLine();
```

Denna kod läser en rad i taget från vår textfil och tilldelar den till en `String` variabel. Om vi vill läsa hela filen kan vi använda en loop för att läsa varje rad, som i exemplet nedan:

```Java
String line;
while ((line = reader.readLine()) != null) {
    // Gör något med varje rad här
}
```

När vi är klara med att läsa filen, måste vi stänga vår `BufferedReader` för att frigöra resurser. Detta görs enkelt genom att skriva:

```Java
reader.close();
```

## Djupdykning

När vi läser en textfil är det viktigt att förstå filens format. Om det är en vanlig textfil med endast tecken och rader, då är `readLine()` metoden tillräcklig för att läsa den. Men ibland kan filen innehålla annan data, som till exempel kommaseparerad data eller XML-format. I sådana fall måste vi använda andra metoder och tekniker för att läsa in och bearbeta datan.

Vi kan också använda `FileWriter` och `PrintWriter` objekt för att skriva till en textfil. Detta kan vara användbart om vi vill skapa eller uppdatera en fil.

## Se även

- [Java File Handling Tutorial](https://www.javatpoint.com/java-file)
- [Oracle Java File Class Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
- [Java Write to File Tutorial](https://www.geeksforgeeks.org/write-data-file-java/)