---
title:                "Java: Å finne lengden av en streng"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden til en streng er en vanlig oppgave for mange Java-programmerere, da det ofte er nødvendig å håndtere strenger i ulike situasjoner. Ved å finne lengden til en streng, kan man få informasjon om hvor mange tegn den inneholder, og dermed kunne utføre ulike operasjoner på den.

## Hvordan

For å finne lengden til en streng i Java, kan man bruke metoden `length()` som er tilgjengelig for alle strenger. Dette gjør det enkelt å få tilgang til lengden uten å måtte gjøre komplekse beregninger. La oss se på et eksempel:

```Java
String navn = "Marie";
System.out.println(navn.length());
```

Dette vil gi følgende output:

```
5
```

I dette eksempelet viser `navn.length()` til lengden av strengen `navn`, som i dette tilfellet er 5. Dette inkluderer også mellomrom og spesialtegn.

## Dypdykk

Det er viktig å merke seg at lengden til en streng er forskjellig fra indeksen til en streng. Lengden refererer til antall tegn, mens indeksen refererer til posisjonene til hvert tegn i strengen. For eksempel, hvis vi bruker indeksen 0 på `navn`-strengen, vil vi få bokstaven "M", mens indeks 1 vil gi oss "a". Det er viktig å holde denne forskjellen i tankene for å unngå forvirring.

En annen viktig ting å merke seg er at lengden til en streng vil endre seg hvis vi endrer på strengen. For eksempel, hvis vi legger til en bokstav i strengen `navn`, vil lengden øke med 1. Dette er fordi `length()`-metoden alltid tar hensyn til den nåværende tilstanden til strengen.

## Se også

- [String Class - Oracle Documentation](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [Java Strings Tutorial - W3Schools](https://www.w3schools.com/java/java_strings.asp)
- [Java String length() method - GeeksforGeeks](https://www.geeksforgeeks.org/java-string-length-method-example/)