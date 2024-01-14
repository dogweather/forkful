---
title:    "Java: Skapa en temporär fil"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Varför

Att skapa en temporär fil är ett vanligt verktyg i Java-programmering. Det kan användas för olika ändamål, som att lagra data temporärt eller för att hantera tillfälliga filer. Det är också ett användbart sätt att organisera dina projekt och hantera resurser som inte behövs permanent.

## Så här gör man

```Java
try {
    //Skapa en temporär fil med standardprefix och suffix (.tmp)
    File tempFile = File.createTempFile("tempFile", ".tmp");
    
    //Använd filen som du vill
    //Exempel: Skriv till filen
    FileWriter writer = new FileWriter(tempFile);
    writer.write("Det här är ett exempel på datan som ska lagras i den temporära filen.");
    writer.close();
    
    //Läsa från filen
    BufferedReader reader = new BufferedReader(new FileReader(tempFile));
    String data = reader.readLine();
    System.out.println(data); //Utdata: Det här är ett exempel på datan som ska lagras i den temporära filen.
    
    //Radera filen efter användning
    tempFile.delete();
    
} catch (IOException e) {
    e.printStackTrace();
}
```

I koden ovan skapar vi en temporär fil med hjälp av metoden `createTempFile()` från klassen `File`. Vi anger ett prefix och suffix för filen, men dessa är valfria. Sedan kan vi skriva och läsa från filen som vilken vanlig fil som helst. När vi är klara använder vi metoden `delete()` för att radera filen från systemet.

Du kan också ange en specifik mapp där du vill att den temporära filen ska skapas genom att använda en överlagring av `createTempFile()` som tar in en sökvägssträng som en parameter.

```Java
File tempFile = File.createTempFile("tempFile", ".tmp", "C:\\temp");
```

## Djupdykning

När du använder metoden `createTempFile()` måste du vara medveten om att filen skapas i det lokala temporära mappsystemet som är dedikerat för din Java-applikation. Detta betyder att filen kommer att raderas automatiskt när din Java-applikation avslutas.

Filen som skapas är också unik och kommer att ha ett annat namn varje gång din applikation körs. Detta är viktigt att tänka på om du planerar att återanvända och referera till samma temporära fil i din kod.

## Se även

- [File klassen dokumentation](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/io/File.html)
- [Java IOExceptions - Hantera input/output fel](https://www.javatpoint.com/exception-handling-in-java)
- [Java I/O tutorial](https://www.tutorialspoint.com/java/java_files_io.htm)