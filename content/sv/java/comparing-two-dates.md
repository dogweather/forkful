---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Jämförelse av två datum i programmering innebär att avgöra vilket datum som kommer före eller efter. Detta är oumbärligt när du spårar händelser, hanterar reservationssystem, upprättar tidsplaner med mera.

## Hur:

Här är ett exempel på hur man jämför två datum i Java med hjälp av `isAfter()`, `isBefore()`, och `isEqual()` metoder.

```Java
import java.time.LocalDate;

public class Main {
  public static void main(String[] args) {

    LocalDate date1 = LocalDate.of(2022, 1, 1);
    LocalDate date2 = LocalDate.of(2022, 1, 2);

    System.out.println("Datum1 är efter datum2: " + date1.isAfter(date2));
    System.out.println("Datum1 är före datum2: " + date1.isBefore(date2));
    System.out.println("Datum1 är samma som datum2: " + date1.isEqual(date2));
  }
}
```
Utskriftsresultaten kommer att vara:
```Java
Datum1 är efter datum2: false
Datum1 är före datum2: true
Datum1 är samma som datum2: false
```

## Djupdykning

Historisk kontext: I äldre versioner av Java var `java.util.Date` och `java.util.Calendar` klasserna som användes för att hantera datum. Men de hade problem som brist på säkerhet och otillfredsställande API-design. Med Java 8 introducerades det nyare `java.time.*` paket som ger mer robusta, trådsäkra och omfattande datum och tidshanteringsfunktioner.

Alternativ: Andra alternativ till att jämföra datum inkluderar jämförelse av millisekunder sedan Unix Epoch (1 januari 1970) eller användning av `compareTo()` metod som returnerar 0 om datum är lika, ett negativt värde om det första datumet förekommer före det andra och ett positivt värde i omvänd situation.

Implementeringsdetaljer: `isAfter()`, `isBefore()`, och `isEqual()` metoder jämför datum på basen av år, månad och dag. De tar inte hänsyn till tidzonen.

## Se Även:

1. Javas offentliga dokumentation om 'java.time.LocalDate' [här](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html).
2. Java 8 datum och tidshantering, praktisk guide [här](https://www.baeldung.com/java-8-date-time-intro).
3. Javas offentliga dokumentation om 'java.util.Date' [här](https://docs.oracle.com/javase/7/docs/api/java/util/Date.html).