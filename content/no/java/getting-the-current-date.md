---
title:    "Java: Få dagens dato"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Hvorfor

Å få nåværende dato kan være utrolig nyttig når du programmerer i Java. Datoer brukes i en rekke applikasjoner, fra å vise tidspunktet for en hendelse til å beregne tidsintervaller. Å kunne hente nåværende dato i Java gir deg muligheten til å utvikle mer dynamiske og funksjonelle programmer.

## Hvordan

For å få nåværende dato i Java, kan du bruke klassen `java.util.Date`. Denne klassen representerer et datobjekt som inneholder informasjon om nåværende dato og tid. Her er et eksempel på hvordan du kan bruke denne klassen:

```Java
import java.util.Date;

public class CurrentDateExample {
    public static void main(String[] args) {
        Date currentDate = new Date();
        System.out.println("Nåværende dato er: " + currentDate);
    }
}
```

Dette vil gi følgende utskrift:

```
Nåværende dato er: Wed May 12 12:00:00 CEST 2021
```

## Dypdykk

I tillegg til å få nåværende dato og tid, kan du også bruke `DateTimeFormatter`-klassen til å formatere datoen på ønsket måte. Dette lar deg tilpasse utseendet til datoen, for eksempel ved å vise den som bare en dato eller bare en tid. Her er et eksempel på hvordan du kan gjøre dette:

```Java
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class CurrentDateTimeFormatterExample {
    public static void main(String[] args) {
        LocalDateTime now = LocalDateTime.now();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm:ss");
        String formattedDateTime = now.format(formatter);

        System.out.println("Nåværende dato og tid er: " + formattedDateTime);
    }
}
```

Dette vil gi følgende utskrift:

```
Nåværende dato og tid er: 12/05/2021 12:00:00
```

## Se også

- [Java Date and Time](https://docs.oracle.com/javase/tutorial/datetime/)
- [Java SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Date and Time API for Java 8](https://www.baeldung.com/java-8-date-time-intro)