---
title:                "Java: Å finne lengden på en streng"
simple_title:         "Å finne lengden på en streng"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Java-programmering er en av de mest populære og allsidige programmeringsspråkene i dag. En av de mest grunnleggende oppgavene i Java-programmering er å finne lengden på en streng. Dette kan virke som en enkel oppgave, men dypdykking i hvordan dette fungerer kan hjelpe deg med å forstå de grunnleggende prinsippene i Java-programmering og gjøre deg til en bedre programmerer.

## Hvordan

For å finne lengden på en streng i Java, bruker vi `length()` metoden. Denne metoden returnerer antall tegn i en streng og kan brukes både på en vanlig streng eller en strengvariabel. La oss se på et eksempel:

```Java
String navn = "Per";
System.out.println(navn.length());
```

Dette vil gi følgende utgang:

```
3
```

Som du kan se, returnerte `length()` metoden verdien 3, som er antall tegn i strengen "Per". La oss se på et annet eksempel der vi bruker `length()` metoden på en strengvariabel:

```Java
String setning = "Jeg liker å programmere";
System.out.println(setning.length());
```

Dette vil gi følgende utgang:

```
23
```

Her returnerte `length()` metoden verdien 23, som er antall tegn i strengen "Jeg liker å programmere". Det er viktig å merke seg at mellomrom også telles som tegn. Du kan også bruke `length()` metoden på tomme strenger, som vil returnere verdien 0.

## Dypdykk

Når det gjelder å finne lengden på en streng, er det flere ting å være oppmerksom på. For det første, som nevnt tidligere, telles mellomrom også som tegn og øker dermed lengden på en streng. I tillegg, hvis vi ønsker å finne lengden på en ledetekst (eller et tegn) i stedet, må vi bruke `length()` metoden med `toUpperCase()` eller `toLowerCase()` metoden. La oss se på et eksempel der vi ønsker å finne lengden på bokstaven 'a':

``` Java
String bokstav = "A";
System.out.println(bokstav.length());
```

Dette vil gi følgende utgang:

```
1
```

Som du kan se, returnerte `length()` metoden verdien 1, som er antall tegn i strengen "A". Dette skyldes at vi brukte `length()` metoden på en ledetekst og ikke en streng med flere tegn.

## Se også

- [Java String Class Reference](https://www.javatpoint.com/string-length)
- [Java String API Documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--)
- [Tutorialspoint: Java String Length](https://www.tutorialspoint.com/Java-String-length)