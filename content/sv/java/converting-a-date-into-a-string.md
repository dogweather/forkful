---
title:                "Java: Omvandling av ett datum till en sträng"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

#Varför Konvertera Ett Datum Till En Sträng

Att konvertera ett datum till en sträng är en vanlig uppgift inom programmering, och det finns flera anledningar till varför du kanske behöver göra detta. För det första kan det vara användbart när du vill visa datumet i ett läsbart format för användaren. Det kan också vara nödvändigt för att lagra datumet i en databas eller skicka det som en parameter till en annan funktion.

##Så Här Gör Du

För att konvertera ett datum till en sträng använder vi oss av klassen `SimpleDateFormat` i Java. Vi börjar med att skapa ett objekt av denna klass, och därefter använder vi metoden `format()` för att konvertera datumet till en sträng.

```Java
SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
Date date = new Date();
String dateString = dateFormat.format(date);
```

I det här exemplet skapar vi ett objekt av `SimpleDateFormat` som tar emot ett argument i form av ett format för datumet. Vi använder formatet "dd/MM/yyyy" för att visa datumet i formatet dag/månad/år. Sedan skapar vi ett objekt av klassen `Date`, som representerar dagens datum och tid. Slutligen använder vi `format()`-metoden för att konvertera datumet till en sträng.

Output:
```11/05/2021```

##Djupare Förklaring

Det finns flera olika format som kan användas när man vill konvertera ett datum till en sträng. Här är några exempel:

- `dd/MM/yyyy`: dag/månad/år (ex. 11/05/2021)
- `MM/dd/yyyy`: månad/dag/år (ex. 05/11/2021)
- `yyyy-MM-dd`: år/månad/dag (ex. 2021-05-11)
- `EEE, MMM dd yyyy`: veckodag, månad, dag, år (ex. Tues, May 11 2021)

Tillgängliga format är dock beroende på vilka locale inställningar som är satta i ditt program. Du kan också använda `SimpleDateFormat`-klassen för att konvertera en sträng tillbaka till ett datumobjekt med hjälp av `parse()`-metoden.

##Se Även

För mer information om `SimpleDateFormat` kan du besöka dessa länkar:
- [Java Dokumentation - SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [W3Schools - SimpleDateFormat](https://www.w3schools.com/java/java_date_formats.asp)
- [Baeldung - Java Date and Time API](https://www.baeldung.com/java-date-time-api)

#Se Även

För mer information om Java programmering och andra användbara tips och tricks, besök dessa länkar:
- [Java Developer - Java Programmering](https://www.javadeveloper.nu/java-programmering/)
- [TutorialsPoint - Java Tutorial](https://www.tutorialspoint.com/java/index.htm)
- [GeeksforGeeks - Java Programming Language](https://www.geeksforgeeks.org/java/)