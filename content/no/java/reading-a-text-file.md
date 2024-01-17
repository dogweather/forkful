---
title:                "Å lese en tekstfil"
html_title:           "Java: Å lese en tekstfil"
simple_title:         "Å lese en tekstfil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Når vi snakker om å lese en tekstfil, mener vi å ta informasjon fra en tekstfil og behandle den i vårt Java-program. Dette er en vanlig oppgave for mange programmører, siden tekstfiler er en vanlig måte å lagre og organisere data på. Ved å lese en tekstfil kan vi få tilgang til denne dataen og bruke den til å utføre forskjellige oppgaver i vårt program.

## Hvordan:
Vi kan lese en tekstfil ved å bruke klassebiblioteket FileReader i Java. Først må vi opprette en instans av FileReader og angi navnet på tekstfilen vi vil lese. Deretter kan vi bruke en Scanner til å lese informasjonen fra tekstfilen og behandle den. Her er et eksempel på hvordan dette kan gjøres:

```Java
import java.io.FileReader;
import java.util.Scanner;

public class LeseTekstfil {
  public static void main(String[] args) {
    try {
      // Opprett en instans av FileReader med navnet på tekstfilen
      FileReader filLeser = new FileReader("minTekstfil.txt");
      
      // Bruk en Scanner til å lese informasjon fra tekstfilen
      Scanner leser = new Scanner(filLeser);
      
      // Les linje for linje fra tekstfilen og skriv ut informasjonen
      while (leser.hasNextLine()) {
        String linje = leser.nextLine();
        System.out.println(linje);
      }
      
      // Lukk Scanner og FileReader for å frigjøre ressurser
      leser.close();
      filLeser.close();
      
    } catch (Exception e) {
      System.out.println("Kunne ikke lese tekstfilen.");
      e.printStackTrace();
    }
  }
}
```

Eksempel output:

```
Dette er en tekstfil.
Den inneholder litt informasjon.
Programmet har nå lest denne informasjonen og skrevet den ut.
```

## Dykk dypere:
Å lese en tekstfil har vært en vanlig oppgave for Java-programmerere siden begynnelsen av språket. Tidligere var det vanlig å bruke BufferedReader-klassen i stedet for Scanner-klassen for å lese tekstfiler. Det finnes også andre måter å lese tekstfiler på, for eksempel ved bruk av InputStream og InputStreamReader-klassene. Det er viktig å merke seg at ved å bruke Scanner, vil alle tegn som blir lest bli konvertert til Unicode-format, mens ved bruk av BufferedReader vil tegnene bli lest direkte fra teksten i filen.

## Se også:
- [Oracle dokumentasjon for FileReader](https://docs.oracle.com/javase/10/docs/api/java/io/FileReader.html)
- [Tutorialspoint tutorial on reading text files in Java](https://www.tutorialspoint.com/java/io/java_io_filereader.htm)
- [Java API: Scanner Class](https://www.geeksforgeeks.org/scanner-class-in-java/)