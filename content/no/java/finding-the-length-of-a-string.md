---
title:    "Java: Å finne lengden på en streng"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Hvorfor
Å finne lengden til en streng kan være nyttig i mange programmeringssammenhenger. Det kan hjelpe deg med å håndtere tekstinput fra brukere, manipulere tekst eller sjekke om to strenger er like.

## Slik gjør du det
For å finne lengden til en streng i Java, kan du bruke metoden `length()` på en `String` variabel. La oss se på et eksempel hvor vi vil finne lengden til strengen "Hei, verden!":

```Java
String streng = "Hei, verden!";
int lengde = streng.length();
System.out.println("Lengden på strengen er " + lengde);
```

Dette vil gi følgende utskrift:

`Lengden på strengen er 12`

Som du kan se, returnerte `length()`-metoden antall tegn i strengen.

## Dypdykk
Når vi bruker `length()`-metoden, så teller den også med mellomrom og spesialtegn, ikke bare bokstaver og tall. Det betyr at lengden til "Hei, verden!" er 12, selv om det bare er 11 bokstaver og tall.

Det kan også være lurt å være forsiktig med hvordan du håndterer lengden til en streng i henhold til indeksering. I Java, og mange andre programmeringsspråk, så begynner indeksering på 0, så den første bokstaven i en streng har indeks 0, den andre har indeks 1, og så videre. Det betyr at hvis du vil ha tak i den siste bokstaven i en streng, må du bruke indeksen `lengde - 1`.

## Se også
- [Java String documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java String length() method](https://www.geeksforgeeks.org/string-length-method-in-java-with-examples/)
- [Java String manipulation tutorial](https://www.baeldung.com/java-string-manipulation)