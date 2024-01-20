---
title:                "Omvandla ett datum till en sträng"
html_title:           "C#: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför? 
Att konvertera ett datum till en sträng i Java innebär att förändra datatypen från ett Datum-objekt till en textsträng i ett specifikt format. Programutvecklare gör detta för att det är lättare att arbeta med och visa data i textform.

## Hur man gör:
Här är en enkel kodsnutt för att konvertera ett datum till en sträng i Java:
``` Java
import java.text.SimpleDateFormat; 
import java.util.Date; 

public class Main {
  public static void main(String[] args) { 
    Date date = new Date(); 
    SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss"); 
    String strDate = formatter.format(date); 
    System.out.println("Date Format with dd-MM-yyyy HH:mm:ss : "+ strDate);
  } 
}
```
När du kör denna kod, kommer utskriften att se ut så här (datum och tid variabler beroende på när du kör koden):
``` Java
Date Format with dd-MM-yyyy HH:mm:ss : 22-02-2022 14:23:45
```
## Djupdykning: 

1. Historiskt: Funktionen för att konvertera datum till en sträng infördes i Java 1.1-biblioteket som en del av java.text.SimpleDateFormat klassen.

2. Alternativ: Du kan också använda Java 8 LocalDate och DateTimeFormatter-klasserna för att göra konverteringen effektivare och säkrare.

3. Implementation detaljer: SimpleDateFormat-klassen i Java använder särskilda formatteringssymboler för att definiera hur det konverterade datumet ska se ut. Till exempel betyder 'dd' dagen i månaden, 'MM' betyder månaden, 'yyyy' betyder året, 'HH' betyder timmen, 'mm' betyder minuten, och 'ss' betyder sekunden.

## Se även:
Länkar till ytterligare resurser finns nedan: 

1. Oracle Java Docs: [SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html) 
2. Oracle Java Docs: [Date](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html) 
3. StackOverFlow: [How to convert Date to String in Java](https://stackoverflow.com/questions/5683728/convert-java-util-date-to-string)
4. Oracle Java Docs: [DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)