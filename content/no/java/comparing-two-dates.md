---
title:                "Sammenligne to datoer"
html_title:           "Java: Sammenligne to datoer"
simple_title:         "Sammenligne to datoer"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bør man sammenligne to datoer i Java? Datoer er en viktig del av mange programmer og å kunne sammenligne dem er nøkkelen til å validere data og gjøre beregninger basert på tid. Det er også nyttig i programmering for å lage betingelser og logikk basert på datoer.

## Hvordan

For å sammenligne to datoer i Java, kan du bruke klassen "LocalDate". Først må du importere denne klassen ved å legge til følgende linje øverst i koden din:

```Java
import java.time.LocalDate;
```
Deretter kan du opprette to LocalDate-objekter med de to datoene du vil sammenligne:

```Java
LocalDate date1 = LocalDate.of(2021, 1, 1);
LocalDate date2 = LocalDate.of(2021, 5, 1);
```
Du kan deretter bruke metoden "isEqual" for å sjekke om de to datoene er like:

```Java
boolean isEqual = date1.isEqual(date2); // vil returnere false i dette tilfellet
```

Du kan også bruke metoden "isAfter" eller "isBefore" for å sjekke om en dato kommer før eller etter en annen:

```Java
boolean isAfter = date1.isAfter(date2); // vil returnere false i dette tilfellet
boolean isBefore = date1.isBefore(date2); // vil returnere true i dette tilfellet
```

Dette er et enkelt eksempel, men du kan også inkludere andre parametere som timer, minutter og sekunder i datoene for å få en mer nøyaktig sammenligning.

## Deep Dive

Det er viktig å være oppmerksom på forskjellen mellom "LocalDate" og "LocalDateTime" i Java når man sammenligner datoer. LocalDate brukes til å representere et bestemt kalenderdato, mens LocalDateTime inkluderer en tidskomponent og kan brukes til å representere en spesifikk tid på dagen.

En annen viktig ting å huske på er at LocalDate er en uforanderlig klasse, og at metodene som brukes for å sammenligne datoer vil returnere en ny instans av LocalDate-objektet i stedet for å endre det opprinnelige objektet.

Det er også verdt å merke seg at LocalDate-objekter kan konverteres til andre datoformater, som "Calendar", ved hjelp av metoden "toDate" eller "toInstant".

## Se også

- [LocalDate dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java Date and Time API](https://www.baeldung.com/java-8-date-time-intro)