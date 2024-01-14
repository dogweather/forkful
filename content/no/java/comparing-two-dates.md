---
title:                "Java: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sammenligne to datoer kan være en viktig del av mange programmeringsoppgaver. Dette kan være nyttig for å sjekke om en dato kommer før eller etter en annen, eller for å se om en dato er innenfor et visst tidsintervall. Å forstå hvordan man sammenligner datoer kan derfor være nyttig for å skrive robust og nøyaktig kode.

## Hvordan gjøre det

Før vi dykker inn i hvordan man sammenligner to datoer, må vi først forstå hvordan Java representerer datoer. I Java brukes klassen "Date" for å representere en bestemt dato og klokkeslett. For å sammenligne to datoer bruker vi metoden "compareTo()", som returnerer et heltall avhengig av forholdet mellom de to datoene. Hvis den første datoen er tidligere enn den andre, vil metoden returnere et negativt tall. Hvis den andre datoen er tidligere, vil den returnere et positivt tall. Hvis datoene er like, vil metoden returnere 0.

La oss se på et eksempel for å få bedre forståelse:

```Java
// Oppretter to Date objekter
Date firstDate = new Date(2021, 5, 12);
Date secondDate = new Date(2021, 5, 15);

// Sammenligner datoene
int result = firstDate.compareTo(secondDate);

// Sjekker resultatet
if (result < 0) {
    System.out.println("Første dato kommer før andre dato.");
} else if (result > 0) {
    System.out.println("Første dato kommer etter andre dato.");
} else {
    System.out.println("Datoene er like.");
}
```

Dette vil gi følgende utskrift:

```
Første dato kommer før andre dato.
```

Som du kan se, har vi sammenlignet to datoer ved å bruke "compareTo()" metoden og fått et resultat avhengig av forholdet mellom dem. Dette kan være nyttig for å implementere logikk basert på datoer i programmer.

## Dykke dypere

Nå som vi har en grunnleggende forståelse av hvordan man sammenligner datoer i Java, kan vi dykke litt dypere og se på andre faktorer som kan påvirke sammenligningen. En viktig ting å merke seg er at metoden "compareTo()" sammenligner ikke bare datoene, men også klokkeslettet. Dette betyr at hvis klokkeslettet for to datoer er forskjellige, vil metoden returnere et annet tall uavhengig av datoenes forhold.

Det er også verdt å merke seg at klassen "Date" har blitt erstattet av klassen "LocalDate" i nyere versjoner av Java. I motsetning til "Date" klassen, beholder "LocalDate" kun informasjon om dato og ikke klokkeslett. Dette kan være nyttig hvis man kun ønsker å sammenligne datoer og ikke bryr seg om klokkeslettet.

## Se også

For mer informasjon om hvordan man sammenligner datoer i Java, kan følgende ressurser være nyttige:

- [Java Date Documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java LocalDate Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java Date and Time Tutorial](https://www.baeldung.com/java-date-time-comparison)