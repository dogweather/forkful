---
title:    "Java: Få aktuellt datum"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
Det finns många anledningar till varför man skulle vilja få den nuvarande datumet i en Java-applikation. Det kan vara för att skriva ut det på en användargränssnitt, spara det i en databas eller använda det för att utföra vissa beräkningar. Oavsett vilken anledning som finns, är det en viktig del av många programmeringsuppgifter.

## Hur man gör det
För att få den nuvarande datumet i Java, kan man använda sig av klassen "LocalDate". Detta är en del av "java.time" biblioteket som introducerades i Java 8. Genom att skapa en instans av detta objekt, kan man få tillgång till olika metoder för att hämta datumet, månaden, året osv. Här är ett enkelt exempel:

```Java
LocalDate currentDate = LocalDate.now();
System.out.println("Idag är det " + currentDate);
```
Output: Idag är det 2021-03-04

Det är också möjligt att formatera datumet enligt ens preferenser med hjälp av "DateTimeFormatter" klassen. Till exempel kan man skriva om det föregående exemplet för att få en mer läsbar output:

```Java
LocalDate currentDate = LocalDate.now();
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd MMM yyyy");
String formattedDate = currentDate.format(formatter);
System.out.println("Idag är det " + formattedDate);
```
Output: Idag är det 04 Mar 2021

## Djupdykning
För de som är intresserade av en djupare förståelse för hur Java hanterar datum och tider, finns det flera andra klasser och metoder som kan användas. Till exempel kan man använda "LocalDateTime" för att få tiden också, eller "ZonedDateTime" för att hantera tidszoner. Det finns också olika metoder för att jämföra datum och tider, beräkna skillnader mellan dem och mycket mer.

Det är också viktigt att notera att "LocalDate" klassen inte är trådsäker, vilket betyder att den inte lämpar sig för flertrådiga applikationer där flera trådar kan försöka ändra datumet samtidigt. I sådana fall bör man använda sig av "Instant" eller "Calendar" klasserna som har inbyggd trådsäkerhet.

## Se även
Här är några användbara resurser för att lära sig mer om Java och hantering av datum och tider:
- Officiell Java-dokumentation för "java.time" biblioteket: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Java Tutorials - Date Time API: https://docs.oracle.com/javase/tutorial/datetime/index.html
- JournalDev - Java Date Time API: https://www.journaldev.com/2800/java-8-date-localdate-localdatetime-instant

Hoppas detta har varit en hjälpsam introduktion till hur man får den nuvarande datumet i Java. Lycka till med dina programmeringsprojekt!