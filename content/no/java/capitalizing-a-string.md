---
title:    "Java: Store bokstaver i en streng"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen bry seg om å gjøre om en vanlig streng til en som er skrevet med store bokstaver? Vel, det er faktisk en ganske vanlig oppgave når du jobber med tekstbehandling. Enten du trenger å lage en tittel eller bare vil sikre at teksten din er konsekvent, å gjøre om en streng til store bokstaver er en nyttig funksjon å ha i din programmeringsverktøykasse.

## Hvordan gjøre det

Så hvordan kan du gjøre dette i Java? Heldigvis finnes det en innebygd metode som heter "toUpperCase()" som gjør akkurat det vi trenger. Her er et eksempel på hvordan du kan bruke denne metoden på en streng:

```Java
String tekst = "dette er en test";
String storeBokstaver = tekst.toUpperCase(); //variabelen "storeBokstaver" vil nå inneholde "DETTE ER EN TEST"
System.out.println(storeBokstaver); //dette vil skrive ut "DETTE ER EN TEST" i konsollen
```

Som du kan se, er det ganske enkelt å gjøre om en streng til store bokstaver ved hjelp av "toUpperCase()" metoden. Du kan også bruke denne metoden på enkelte bokstaver i en streng, ved å bruke "charAt()" metoden til å peke på det spesifikke tegnet du vil endre.

## Dypdykk

For de som ønsker å lære mer om hvordan denne metoden fungerer, er det verdt å nevne at den tar hensyn til ulike språk og bokstavtyper. For eksempel vil den riktige store bokstaven for "ø" i det norske alfabetet være "Ø" og ikke "O", slik at Java-metoden håndterer dette riktig. Det er også viktig å merke seg at denne metoden kun endrer bokstaver som allerede er små, så hvis du prøver å bruke den på en streng som allerede inneholder store bokstaver, vil den ikke ha noen effekt.

## Se også

- [Java String class](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java Character class](https://docs.oracle.com/javase/8/docs/api/java/lang/Character.html)
- [Java Methods](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#methods)