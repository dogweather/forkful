---
title:                "Java: Konvertere en streng til små bokstaver"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en streng til små bokstaver er en vanlig oppgave for utviklere som arbeider med tekstbehandling eller programmering. Det er viktig for å sikre konsistens og sammenlignbarhet i teksten, uavhengig av hva slags data som kommer inn.

## Hvordan

For å konvertere en streng til små bokstaver, kan du bruke metoden `toLowerCase()` i Java. Denne metoden tar ingen parametere og returnerer en ny streng hvor alle bokstavene er konvertert til små bokstaver.

```Java
String input = "DETTE ER EN TEST"; 
String output = input.toLowerCase(); 

System.out.println(output); 
```

Output vil bli `dette er en test`. Som du ser, er alle bokstavene konvertert til små bokstaver.

## Dypdykk

Det er viktig å merke seg at konverteringen til små bokstaver er basert på Locale-innstillingene på maskinen hvor koden kjører. Dette betyr at det er mulig for en streng å bli konvertert til forskjellige former av små bokstaver, avhengig av hvor den blir utført.

En annen ting å merke seg er at `toLowerCase()` kun konverterer alfabeteriske bokstaver, og ikke tall, symboler eller spesielle tegn.

For å få bedre forståelse for hvordan `toLowerCase()` fungerer, kan du se på koden for metoden i Java-kildekoden, eller sjekke ut dokumentasjonen for mer informasjon.

## Se også

- [Java String dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- [Java Locale dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html)
- [Java-håndbok for konvertering av strenger til store og små bokstaver](https://docs.oracle.com/javase/tutorial/i18n/text/converting.html)