---
title:    "Java: Konvertere en streng til små bokstaver"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Hvorfor

Når du jobber med Java-programmering, vil du ofte støte på situasjoner der du trenger å behandle tekststrenger. En vanlig oppgave er å konvertere en tekststreng til små bokstaver, eller "lower case" på engelsk. Dette er nyttig for å sammenligne eller manipulere strenger på en mer konsistent måte. I denne bloggposten vil vi vise deg hvordan du kan gjøre dette på en enkel måte i Java.

## Hvordan

For å konvertere en tekststreng til små bokstaver i Java, bruker vi metoden `toLowerCase()` fra `String`-klassen. Her er et eksempel på hvordan du kan bruke denne metoden i koden din:

```Java
String originalStreng = "HEI PÅ DEG";
String strengMedSmåBokstaver = originalStreng.toLowerCase();
System.out.println(strengMedSmåBokstaver);
```

I dette eksempelet har vi en tekststreng med store bokstaver, og vi ønsker å konvertere den til små bokstaver. Når vi kjører koden, vil vi få følgende output:

```
hei på deg
```

Som du kan se, har metoden `toLowerCase()` konvertert alle bokstaver til små bokstaver. Det er også verdt å merke seg at denne metoden ikke endrer den opprinnelige tekststrengen, men returnerer en helt ny tekststreng med de konverterte bokstavene.

## Deep Dive

Når du bruker metoden `toLowerCase()` i Java, er det viktig å vite at den følger Unicode-standarden. Dette betyr at den vil håndtere alle språk og spesielle tegn på en korrekt måte. For eksempel, hvis du har en tekststreng på tysk eller norsk, vil den håndtere spesialtegn som "ü" eller "ø" riktig ved å konvertere dem til de tilsvarende små bokstavene.

Det er også verdt å merke seg at denne metoden er "case-sensitive". Det betyr at den bare vil konvertere store bokstaver til små bokstaver, ikke omvendt. Hvis du har en tekststreng med både små og store bokstaver, og bare ønsker å konvertere de store bokstavene, må du bruke en annen metode som `toUpperCase()`.

## Se Også

- [Java String Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java Unicode Character Codes](https://unicode-table.com/en/)
- [Java String Methods](https://www.w3schools.com/java/java_ref_string.asp)