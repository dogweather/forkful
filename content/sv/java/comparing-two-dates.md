---
title:    "Java: Jämföra två datum"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum kan verka som en enkel uppgift, men det finns flera situationer där det är viktigt att kunna göra det korrekt. Till exempel kan det behövas för att bestämma födelsedagar eller för att kontrollera giltigheten på ett låneavtal.

## Hur man gör 
För att kunna jämföra två datum i Java kan man använda sig av klassen "LocalDate" som finns i Java 8 och senare versioner. Här är ett enkelt exempel på hur man kan jämföra två datum:

```Java
LocalDate datum1 = LocalDate.of(2020, 02, 15);
LocalDate datum2 = LocalDate.of(2019, 12, 20);
if (datum1.isAfter(datum2)) {
    System.out.println("Datum 1 kommer efter datum 2.");
} else if (datum1.isBefore(datum2)) {
    System.out.println("Datum 1 kommer före datum 2.");
} else {
    System.out.println("Datumen är lika.");
}
```

Output:

> Datum 1 kommer efter datum 2.

I det här exemplet kan man se att vi först deklarerar två olika datum med hjälp av "LocalDate.of()", och sedan använder vi metoden "isAfter()" och "isBefore()" för att jämföra dem. Om man istället vill jämföra tidpunkter kan man använda klassen "LocalDateTime" på samma sätt.

## Djupdykning
När man jämför två datum är det viktigt att vara medveten om att det finns flera olika sätt att göra det på beroende på vilket resultat man vill få. Till exempel kan man använda sig av metoden "isEqual()" för att kontrollera om två datum är exakt lika, eller "compareTo()" för att få en numerisk jämförelse av två datum.

Man bör även vara uppmärksam på tidszoner när man jämför datum och tider för att undvika eventuella felaktiga resultat. Det är också viktigt att vara medveten om hur datum och tider representeras i Java, eftersom detta kan påverka hur man jämför dem.

## Se även
Här är några användbara resurser för att lära sig mer om att jämföra datum i Java:

- [Java LocalDate API dokumentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java LocalDateTime API dokumentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
- [Java Date and Time tutorials på Baeldung](https://www.baeldung.com/java-date-time)