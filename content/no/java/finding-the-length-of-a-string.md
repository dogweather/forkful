---
title:    "Java: Finn lengden av en streng"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden til en streng er en vanlig oppgave i Java-programmering. Dette er nyttig når du for eksempel trenger å validere brukerinput eller håndtere tekstbaserte data.

## Hvordan

Det finnes flere måter å finne lengden til en streng i Java på, men den enkleste er å bruke metoden `length()`. Denne metoden returnerer antall tegn i strengen. La oss se på et eksempel:

```java
String navn = "Per";
int lengde = navn.length();
System.out.println(lengde);
```

Dette vil gi følgende output:

```
3
```

Her ser vi at metodens returnerte verdi er 3, siden det er 3 tegn i strengen "Per". Vi kan også bruke `length()` sammen med en tom streng, da vil returnert verdi være 0:

```java
String tom = "";
int tomLengde = tom.length();
System.out.println(tomLengde);
```

Dette vil gi følgende output:

```
0
```

En annen måte å finne lengden til en streng på, er ved å bruke en løkke. La oss se på et eksempel på dette:

```java
String navn = "Marius";
int lengde = 0;

// Løkken kjører så lenge i er mindre enn lengden til strengen
for(int i = 0; i < navn.length(); i++) {
  lengde++;
}

System.out.println(lengde);
```

Dette vil også gi oss et output på 6, siden det er 6 tegn i strengen "Marius". Det kan være nyttig å bruke en løkke hvis man ønsker å gjøre flere operasjoner basert på strengens lengde.

## Deep Dive

Å finne lengden til en streng er en enkel oppgave, men det kan være nyttig å vite at det i Java finnes en rekke andre metoder som kan brukes på strenger. For eksempel kan vi bruke `substring(startIndex, endIndex)` for å returnere en del av en streng basert på indeksen til start- og sluttpunktet vi ønsker å hente ut.

Vi kan også bruke `charAt(index)` for å returnere en enkelt bokstav fra en streng basert på indeksen. Det er også verdt å merke seg at lengden til en streng ikke er det samme som størrelsen på strengen, da størrelsen tar hensyn til andre ting som lagringsplass og tomme tegn.

## Se også

- [Java String API](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [How to Find the Length of a String in Java](https://www.baeldung.com/java-string-length)
- [Java String Methods](https://www.geeksforgeeks.org/java-string-methods/)