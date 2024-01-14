---
title:    "Java: Få den nåværende datoen"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Hvorfor:

Det å kunne få tak i og bruke den nåværende datoen i et Java-program kan være viktig for å holde styr på tidsbegrensede oppgaver eller for å generere unike verdier. Det er også en viktig del av å lage dynamiske og interaktive applikasjoner som kan tilpasse seg brukerens tidsone og lokale tid.

# Hvordan:
```Java
import java.time.LocalDate;

public class CurrentDate {
    public static void main(String[] args) {
        // Bruker LocalDate.now() for å hente nåværende dato
        LocalDate currentDate = LocalDate.now();
        // Utprinting av formatert dato
        System.out.println("Dagens dato er: " + currentDate);
    }
}
```
Eksempel på output:
```
Dagens dato er: 2021-11-08
```

# Dypdykk:
Det er flere måter å få tak i den nåværende datoen i Java, men med introduksjonen av java.time-pakken i Java 8, er det anbefalt å bruke LocalDate-klassen for dato og tid. Denne klassen tilbyr en rekke nyttige metoder for å håndtere datoer, som for eksempel å hente ut spesifikke verdier som dag, måned og år, og å konvertere til forskjellige formater.

Det er viktig å merke seg at LocalDate-klassen er uendret, som betyr at datoer og tider kan manipuleres uten å endre den opprinnelige verdien. For mer avansert håndtering av datoer og tider, kan også LocalDateTime og ZonedDateTime-klassene brukes.

# Se også:
- [Java 8 LocalDate-dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java 8 Date and Time Tutorial](https://www.baeldung.com/java-8-date-time-intro) (engelsk)