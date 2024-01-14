---
title:                "Java: Å få den nåværende datoen"
simple_title:         "Å få den nåværende datoen"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hvordan du kan få den nåværende datoen i Java-programmering? Det kan virke som en enkel oppgave, men det kan være nyttig i mange programmeringsprosjekter. I denne bloggposten skal vi se nærmere på hvorfor og hvordan du kan få den nåværende datoen i Java.

## Slik gjør du det

Først og fremst trenger du å importere "java.util.Date" pakken i Java-koden din. Deretter kan du bruke Date klassen for å få den nåværende datoen. Her er et enkelt eksempel på hvordan du kan gjøre det:

```Java
import java.util.Date;

public class CurrentDateExample {
   public static void main(String[] args) {
      Date currentDate = new Date();
      System.out.println("Den nåværende datoen er: " + currentDate);
   }
}
```

Når programmet kjøres, vil det vise følgende output:

```
Den nåværende datoen er: Fri Sep 03 14:20:57 CEST 2021
```

Som du kan se, vises datoen automatisk i en spesifikk format. Hvis du ønsker å endre formatet, kan du bruke SimpleDateFormat klassen. Her er et eksempel på hvordan du kan gjøre det:

```Java
import java.util.Date;
import java.text.SimpleDateFormat;

public class CurrentDateExample {
   public static void main(String[] args) {
      Date currentDate = new Date();
      SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");  
      System.out.println("Den nåværende datoen er: " + formatter.format(currentDate));
   }
}
```

Output vil se slik ut:

```
Den nåværende datoen er: 03/09/2021
```

Du kan også bruke andre formateringsalternativer for å få datoen til å vises slik du ønsker det.

## Dypdykk

Nå som du vet hvordan du kan få den nåværende datoen i Java, kan det være nyttig å vite litt mer om hvordan datoen blir hentet. Når du bruker Date klassen, blir datoen hentet fra systemklokken på datamaskinen din. Dette betyr at datoen kan variere avhengig av tidssonen og klokkeslettet på datamaskinen din. Det kan også være lurt å håndtere eventuelle unntak som kan oppstå når man prøver å få datoen, som for eksempel hvis det er problemer med systemklokken.

## Se også

- [W3Schools - Java Date Class](https://www.w3schools.com/java/java_date.asp)
- [Oracle - Java SimpleDateFormat Class](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [GeeksforGeeks - Getting current date and time in Java](https://www.geeksforgeeks.org/get-current-date-and-time-in-java/)