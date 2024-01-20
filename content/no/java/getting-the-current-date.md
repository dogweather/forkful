---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å hente den nåværende datoen i Java betyr at du får datoen og klokkeslettet i det øyeblikket programmet ditt kjører. Vi bruker dette ofte å spore tidpunktet for hendelser, som når en bruker gjør en handling, eller for å legge til en tidsstempel i en loggfil.

## Hvordan:
Her er en måte å få den nåværende datoen i Java på:

```Java
import java.time.LocalDate;
...
LocalDate currentDate = LocalDate.now();
System.out.println("Dagens dato er: " + currentDate);
```

Når du kjører denne koden vil den utskrifte:

```Java
Dagens dato er: 2022-03-17
```

## Dypdykk
Historisk sett har Java-programmerere brukt `java.util.Date` for å håndtere datoer. Men `java.time.LocalDate` (introdotusert i Java 8) er nå mer foretrukket på grunn av sin overlegenhet i formatering og tidszonehåndtering.

Som et alternativ, hvis du trenger både dato og klokkeslett, kan du bruke `java.time.LocalDateTime`:

```Java
import java.time.LocalDateTime;
...
LocalDateTime currentDateTime = LocalDateTime.now();
System.out.println("Dato og tidspunkt er nå: " + currentDateTime);
```

Å forstå hvordan Java mottar den nåværende datoen innebærer å vite litt om systemets klokke. Java vil spørre systemets klokke, som fungerer med det interne klokkesignalet, om den nåværende tiden.

## Se Også
For mer informasjon eller å lese mer om emnet, se på linkene nedenfor:

- [Java LocalDate Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java LocalDateTime Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
- [Oracle Tutorial on Date Time](https://docs.oracle.com/javase/tutorial/datetime/)
- [Baeldung article on how to get the current date in Java](https://www.baeldung.com/java-8-date-time-intro)