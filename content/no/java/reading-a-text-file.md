---
title:                "Java: Å lese en tekstfil"
simple_title:         "Å lese en tekstfil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du vil jobbe med tekstfiler i Java, må du lære å lese dem først. Å lese tekstfiler er en viktig ferdighet som kan hjelpe deg med å behandle og manipulere data på en effektiv måte. Dette kan være nyttig for de som jobber med store mengder data eller som ønsker å automatisere repetitive oppgaver.

## Hvordan

For å lese en tekstfil i Java, må du først opprette et FileReader-objekt og et BufferedReader-objekt. Deretter kan du bruke metoden readLine() for å lese linje for linje fra filen og lagre den i en variabel. Her er et eksempel på hvordan du kan lese en tekstfil og skrive ut innholdet:

```Java
FileReader fileReader = new FileReader("tekstfil.txt");
BufferedReader bufferedReader = new BufferedReader(fileReader);
String line = bufferedReader.readLine();
while (line != null) {
    System.out.println(line);
    line = bufferedReader.readLine();
}
bufferedReader.close();
```

Når du kjører koden over, vil det skrives ut innholdet i tekstfilen linje for linje.

## Deep Dive

Det er viktig å huske på at når du leser en tekstfil i Java, vil det returneres en String som må behandles videre. Dette betyr at du kan bruke alle metoder som er tilgjengelige for Strings, for eksempel split() for å dele opp teksten etter bestemte tegn.

Det er også viktig å være klar over forskjellen på en relative og absolutt filsti når du leser tekstfiler i Java. En relativ filsti vil referere til en fil som ligger i samme mappe som Java-filen som leser den, mens en absolutt filsti vil referere til en fil uavhengig av plasseringen til Java-filen. Vær nøye med å angi riktig filsti for å unngå feil.

## Se Også

- [Java FileReader Dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/io/FileReader.html)
- [Java BufferedReader Dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html)
- [Java String Dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)