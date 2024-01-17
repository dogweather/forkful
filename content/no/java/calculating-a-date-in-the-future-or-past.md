---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Java: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å beregne en dato i fremtiden eller fortiden innebærer å bruke programmering til å finne ut når en spesifikk dato vil være basert på et gitt antall dager i fremtiden eller fortiden. Dette kan være nyttig for å lage tidssensitive applikasjoner eller automatisere oppgaver som krever kunnskap om fremtidige eller tidligere datoer.

## Slik gjør du:
For å beregne en dato i fremtiden eller fortiden i Java, kan du bruke klassen "java.util.Calendar". Følgende eksempel vil vise hvordan du kan finne ut hvilken dato det vil være om 100 dager fra nå:

```java
import java.util.Calendar;

public class DateCalculator {
  public static void main(String[] args) {
  
    // Opprett et Calendar-objekt og sett til dagens dato
    Calendar cal = Calendar.getInstance();
    cal.setTime(new Date());
    
    // Legg til 100 dager på Calendar-objektet
    cal.add(Calendar.DATE, 100);
    
    // Hent ut datoen som et Date-objekt
    Date futureDate = cal.getTime();
    
    // Skriv ut datoen på ønsket format
    SimpleDateFormat format = new SimpleDateFormat("dd/MM/yyyy");
    System.out.println(format.format(futureDate));
  }
}
```

Output:
``` 
08/10/2021
```
Du kan også beregne en dato i fortiden ved å bruke "cal.add(Calendar.DATE, -100);" i stedet for å legge til dager.

## Dykk dypere:
Historisk sett var det vanlig å beregne datoer ved hjelp av kalendere og matematiske beregninger. I dag har programmering gjort det mye enklere ved å tilby innebygde funksjoner som kan håndtere datoer og tidsberegninger.

En alternativ metode for å beregne datoer er å bruke "java.time.LocalDate"-klassen i Java 8. Denne klassen er mer moderne og mer nøyaktig enn "Calendar"-klassen.

Det er også verdt å merke seg at datoberegning kan være mer kompleks hvis du tar hensyn til skuddår, tidsforskjeller og forskjellige tidszoner. Det er viktig å forstå disse aspektene når du utvikler applikasjoner som er avhengige av nøyaktige datoer.

## Se også:
- [Java.util.Calendar Dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [java.time.LocalDate Dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Using Calendar and Date classes in Java](https://www.baeldung.com/java-calendar-date)