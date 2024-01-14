---
title:                "Java: Å skrive en tekstfil"
simple_title:         "Å skrive en tekstfil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor 

Det å skrive en tekstfil i Java kan være en viktig del av programmering. Det kan være nyttig for å lagre data eller kommunisere med brukere.

## Hvordan

For å skrive en tekstfil i Java, kan du bruke klassene `File` og `FileWriter`. Først må du opprette en ny `File`-instans og angi filbanen og navnet på filen du vil skrive. Deretter må du opprette en `FileWriter`-instans og bruke `write()`-metoden til å skrive teksten du ønsker å lagre i filen. Til slutt må du bruke `close()`-metoden for å lagre og lukke filen.

```
Java
  File fil = new File("minFil.txt");
  FileWriter skriver = new FileWriter(fil);
  skriver.write("Dette er en tekstfil skrevet i Java!");
  skriver.close();
```

Dette vil opprette en ny tekstfil med tittelen "minFil.txt" på rotmappen til prosjektet ditt. Du kan også angi en spesifikk filbane hvis du vil at filen skal lagres et annet sted.

## Dypdykk

Det er viktig å huske på at når du bruker `FileWriter`, vil all eksisterende tekst i filen bli erstattet med den nye teksten du skriver. Hvis du vil legge til tekst i en eksisterende fil, kan du bruke `FileWriter`'s andre konstruktør, som tar inn en boolean verdi. Hvis du setter denne til `true`, vil teksten bli lagt til i slutten av filen i stedet for å erstatte eksisterende tekst.

Det er også viktig å huske på at når du bruker `FileWriter`, må du håndtere eventuelle unntak som kan oppstå, for eksempel hvis filen ikke kan opprettes eller skrives til. Dette kan gjøres ved å bruke try/catch-blokker eller kaste unntak.

## Se også

- [Java File-klassen](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
- [Java FileWriter-klassen](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/FileWriter.html)