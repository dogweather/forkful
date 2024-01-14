---
title:    "Java: Beräkna ett datum i framtiden eller förflutna"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna ett datum i framtiden eller det förflutna kan vara en användbar funktion för att hålla koll på viktiga händelser eller planera framtida aktiviteter. Genom att använda Java-programmering kan du enkelt skapa en algoritm för att beräkna datum baserat på olika parametrar.

## Så här gör du

Först och främst måste vi förstå att datum i Java representeras av objektet `LocalDate`. Det finns olika metoder som kan användas för att manipulera en `LocalDate` och beräkna ett önskat datum. Här är ett exempel på en metod som används för att beräkna ett datum i framtiden baserat på ett visst antal år:

```Java
LocalDate idag = LocalDate.now(); // Hämtar dagens datum
int antalÅr = 5; // Antal år vi vill lägga till

// Använder metoden plusYears() för att lägga till år till idag
LocalDate framtidaDatum = idag.plusYears(antalÅr);

// Skriver ut det nya datumet
System.out.println(framtidaDatum);
```

Kör man denna kod kommer man att se att det nya datumet som skrivs ut är 5 år efter dagens datum. Genom att använda metoder som `minusDays()`, `plusMonths()` och `minusYears()` kan man enkelt beräkna datum i både framtiden och förflutnan.

## På djupet

När man beräknar datum i Java måste man också ta hänsyn till skottår och olika kalendrar. Olika länder och kulturer kan ha sina egna kalendrar och datumformat, vilket kan påverka hur man beräknar ett datum. Det är därför viktigt att förstå hur `LocalDate`-objektet fungerar och vilka metoder som finns tillgängliga för att hantera olika situationer.

Ytterligare saker att tänka på när man beräknar datum är att se till att hantera undantag och felhantering, samt att använda rätt format när man skriver ut datumet. Java har olika formateringsklasser som kan användas för att ändra hur datum skrivs ut i konsolen eller i ett GUI.

## Se även

För mer information och exempel på hur man kan beräkna datum i Java, se följande länkar:

- [LocalDate-klassens officiella dokumentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutorialspoint Java Date and Time tutorial](https://www.tutorialspoint.com/java8/java8_date_time.htm)
- [Baeldung Java Date and Time tutorial](https://www.baeldung.com/java-date-time)
- [Java DateTimeFormatter-klassens officiella dokumentation](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)