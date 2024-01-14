---
title:    "Java: Konvertere en streng til små bokstaver"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

I denne blogginnlegget skal vi se nærmere på hvordan du kan konvertere en streng til små bokstaver (lower case) i Java-programmering. Dette er nyttig når du jobber med tekst- og strengmanipulasjon, og kan hjelpe deg med å gjøre din kode mer lesbar og mer effektiv.

## Hvordan gjøre det

For å konvertere en streng til små bokstaver i Java, bruker du metoden `toLowerCase()` i `String`-klassen. Her er et eksempel på hvordan dette kan gjøres:

```Java
String tekst = "HEI PÅ DEG!";
String konvertertTekst = tekst.toLowerCase();
System.out.println(konvertertTekst);
```

Outputen av denne koden vil være `hei på deg!`, hvor alle bokstavene er konvertert til små bokstaver.

## Dykk dypere

Det er viktig å merke seg at denne metoden bare konverterer de bokstavene som allerede er i store bokstaver. Hvis strengen din inneholder tall eller spesialtegn, vil de forbli uendret. Her er et eksempel på dette:

```Java
String tekst = "Hei på deg, jeg heter 123!";
String konvertertTekst = tekst.toLowerCase();
System.out.println(konvertertTekst);
```

Outputen av denne koden vil være `hei på deg, jeg heter 123!`, hvor kun bokstavene er konvertert til små bokstaver. Tallene og spesialtegnene er fortsatt like de var i den opprinnelige strengen.

Det er også verdt å nevne at denne metoden ikke bare fungerer for norske bokstaver, men for alle bokstaver i det latinske alfabetet.

## Se også

Her er noen andre ressurser som kan være nyttige når du jobber med strengmanipulering i Java:

- [Java String API](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [Java String manipulation tutorial](https://www.baeldung.com/java-string-manipulation)
- [Java String methods](https://www.w3schools.com/java/java_ref_string.asp) (med eksempler)