---
title:                "Å lese en tekstfil."
html_title:           "Java: Å lese en tekstfil."
simple_title:         "Å lese en tekstfil."
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du jobber med Java-programmering, er det sannsynligvis at du må lese innholdet av en tekstfil på et eller annet tidspunkt. Dette kan være nyttig for å lese data eller konfigurasjonsfiler, hente informasjon fra en bruker eller for å utføre andre operasjoner som krever tekstbehandling. I denne artikkelen vil vi vise deg hvordan du kan lese en tekstfil i Java på en enkel måte.

## Hvordan

Det første du trenger å gjøre er å opprette en tekstfil på datamaskinen din. Dette kan gjøres ved hjelp av et enkelt tekstredigeringsprogram. I vårt eksempel vil vi lese en fil som heter "testfil.txt".

For å lese innholdet av denne filen i Java, kan du bruke FileInputStream og InputStreamReader klasser. Her er et enkelt eksempel:

```Java
import java.io.*;

public class ReadFile {
  public static void main(String[] args) {
    try {
      // Oppretter en ny fil-inputstrøm og leser tekstfilen
      FileInputStream fis = new FileInputStream("testfil.txt");
      // Oppretter en ny leser for å behandle inputstrømmen
      InputStreamReader isr = new InputStreamReader(fis);
      // Leser en karakter av gangen og skriver ut på konsollen
      int karakter;
      while ((karakter = isr.read()) != -1) {
        System.out.print((char) karakter);
      }
      // Lukker fil-inputstrømmen
      fis.close();
    } catch (IOException e) {
      System.out.println("Feil ved lesing av fil: " + e.getMessage());
    }
  }
}
```

Eksempel på utskrift:

```
Dette er en testfil
Laget av enkelt bruker
for å vise hvordan man kan lese en tekstfil i Java
```

Det første trinnet er å importere java.io-pakken for å bruke nødvendige klasser. Deretter oppretter vi et nytt FileInputStream-objekt og leser filen ved hjelp av InputStreamReader. I while-løkken leser vi en karakter av gangen og bruker metoden read() for å konvertere det numeriske unicode-verdien til en bokstav. Til slutt lukker vi fil-inputstrømmen for å frigjøre ressurser.

## Deep Dive

Det finnes flere måter å lese en tekstfil på i Java, inkludert bruk av Scanner-klassen eller bruk av en BufferedReader. Disse alternativene kan være mer effektive hvis du for eksempel trenger å lese flere linjer av gangen.

Det er også viktig å håndtere eventuelle unntak som kan oppstå under lesing av en tekstfil. I vårt eksempel brukte vi en try-catch blokk for å håndtere IOException, som kan oppstå hvis filen ikke finnes eller det oppstår andre problemer under lesing.

En annen viktig ting å huske på er å lukke fil-inputstrømmen etter at du er ferdig med å lese filen. Dette vil sørge for at eventuelle åpne ressurser blir frigjort og at filen blir lukket på en riktig måte.

## Se også

- [Java FileInputStream dokumentasjon](https://docs.oracle.com/javase/7/docs/api/java/io/FileInputStream.html)
- [Java InputStreamReader dokumentasjon](https://docs.oracle.com/javase/7/docs/api/java/io/InputStreamReader.html)
- [Java Scanner dokumentasjon](https://docs.oracle.com/javase/7/docs/api/java/util/Scanner.html)
- [Java BufferedReader dokumentasjon](https://docs.oracle.com/javase/7/docs/api/java/io/BufferedReader.html)