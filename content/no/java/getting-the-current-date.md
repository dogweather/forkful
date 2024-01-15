---
title:                "Få dagens dato"
html_title:           "Java: Få dagens dato"
simple_title:         "Få dagens dato"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor skulle noen bry seg om å få dagens dato? Vel, i moderne programmering brukes ofte datoer til å spore og registrere hendelser. Dette kan være nyttig for å organisere og analysere data, samt for å lage tidsstempelbaserte rapporter.

## Slik gjør du det
For å få dagens dato i Java, kan du bruke klassen `LocalDate` fra `java.time` pakken. Her er et enkelt eksempel på hvordan du kan skrive ut dagens dato:

```Java
import java.time.LocalDate;

public class DatoEksempel {
    public static void main(String[] args) {
        LocalDate dagensDato = LocalDate.now();
        System.out.println("Dagens dato er: " + dagensDato);
    }
}
```

Koden vil produsere følgende output:

```
Dagens dato er: 2021-10-15
```

Her bruker vi `now()` metoden til å hente dagens dato, og lagrer den i en variabel `dagensDato`. Deretter skriver vi ut datoen ved hjelp av `System.out.println()` metoden.

## Dypdykk
I tillegg til å hente dagens dato, kan du også gjøre mer avanserte operasjoner med `LocalDate` klassen. For eksempel kan du bruke `plus()` metoden til å legge til eller trekke fra et bestemt antall dager, måneder eller år. Her er et eksempel som viser hvordan du kan legge til 7 dager til dagens dato:

```Java
LocalDate dagensDato = LocalDate.now();
dagensDato = dagensDato.plusDays(7);
System.out.println("Datoen om 7 dager er: " + dagensDato);
```

Output vil være:

```
Datoen om 7 dager er: 2021-10-22
```

Du kan også bruke `minus()` metoden på samme måte hvis du vil trekke fra et bestemt antall dager, måneder eller år.

## Se også
* [Java API dokumentasjon for LocalDate](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html)
* [Java Tutorials - Dato og Kalender](https://docs.oracle.com/javase/tutorial/datetime/index.html)