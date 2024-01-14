---
title:                "Java: Sammenføyning av strenger"
simple_title:         "Sammenføyning av strenger"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere strenger, også kjent som konkatinasjon, er et viktig konsept i Java-programmering. Det lar deg sette sammen flere strenger for å lage en lengre streng, som er nyttig for å lage dynamiske meldinger, visning av data og mye mer.

## Hvordan

For å konkatinere strenger i Java, bruker vi "+" -operatøren. Dette lar oss kombinere to eller flere strenger til en enkelt streng. La oss se på et eksempel:

```Java
String navn = "Per";
String alder = "30";
String informasjon = navn + " er " + alder + " år gammel.";
System.out.println(informasjon);
```

Output:
```
Per er 30 år gammel.
```

Vi ser her at vi kombinert tre strenger (variabler) og skapte en lengre streng ved å bruke "+" -operatøren. Merk at mellomrom og tegn må inkluderes i de originale strengene for å få ønsket format i den kombinerte strengen.

## Deep Dive

I tillegg til "+" -operatøren, kan vi også bruke `concat` -funksjonen for å konkatinere strenger i Java. Dette fungerer på samme måte som "+" -operatøren, men vi må huske å legge til en variabel først som skal lagre den kombinerte strengen. Se et eksempel nedenfor:

```Java
String navn = "Kari";
String alder = "25";
String konkatinert = navn.concat(" er ").concat(alder).concat(" år gammel.");
System.out.println(konkatinert);
```

Output:
```
Kari er 25 år gammel.
```

En annen viktig ting å huske på er at i Java er strenger uforanderlige (immutable), det betyr at hvis vi endrer på en streng, vil det opprettes en helt ny streng i minnet. Dette kan påvirke ytelsen til programmet vårt, spesielt når vi jobber med store strenger. Derfor, hvis du jobber med store mengder av data og kombinering av strenger, kan det være lurt å bruke `StringBuilder`-klassen som er optimalisert for å håndtere slike operasjoner.

## Se også

- [Java dokumentasjon: String concatenation](https://docs.oracle.com/javase/tutorial/java/data/manipstrings.html)
- [Tutorialspoint: Java - Strings](https://www.tutorialspoint.com/java/java_strings.htm)
- [GeeksforGeeks: String concatenation in Java](https://www.geeksforgeeks.org/concatenation-in-java/)