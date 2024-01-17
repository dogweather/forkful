---
title:                "Å jobbe med CSV"
html_title:           "Java: Å jobbe med CSV"
simple_title:         "Å jobbe med CSV"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/working-with-csv.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
CSV står for "Comma-Separated Values", og er en vanlig måte å lagre og håndtere data på. Det er et tekstbasert format hvor data er organisert som tabell med kolonner og rader, og verdiene er separert med komma. Programmere bruker CSV fordi det er enkelt å håndtere og kan leses og skrives av de fleste programmer.

# Hvordan:
```java
// Leser en CSV-fil og skriver ut dataene
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class LesCSV {

  public static void main(String[] args) {
    // Oppretter en fil og en filleser
    File fil = new File("data.csv");
    FileReader filleser = null;

    try {
      // Åpner filen og oppretter en filleser
      filleser = new FileReader(fil);

      // Leser filen linje for linje og skriver ut dataene
      int c;
      while ((c = filleser.read()) != -1) {
        System.out.print((char) c);
      }

      // Lukker filleser
      filleser.close();
    } catch (IOException e) {
      System.out.println("Feil ved lesing av fil: " + e.getMessage());
      e.printStackTrace();
    }
  }
}
```
Eksempel på input-fil (data.csv):
```
Fornavn,Etternavn,Alder
Maria,Garcia,24
Juan,Hernandez,32
Emilia,Rodriguez,29
Pablo,Chavez,27
```
Eksempel på output:
```
Fornavn,Etternavn,Alder
Maria,Garcia,24
Juan,Hernandez,32
Emilia,Rodriguez,29
Pablo,Chavez,27
```

# Dykk dypere:
CSV ble opprinnelig utviklet for å lagre data på en enkel måte på maskiner med begrenset kapasitet på 1970-tallet. Alternativer til å håndtere data inkluderer XML og JSON, men CSV er fortsatt et populært valg på grunn av sin enkelhet og lesbarhet. Implementering av CSV kan variere avhengig av programmeringsspråk, men prinsippene er de samme.

# Se også:
- [Wikipedia: Comma-separated values](https://no.wikipedia.org/wiki/Comma-separated_values)
- [Oracle dokumentasjon: Working with CSV Files in Java](https://docs.oracle.com/javase/8/docs/api/java/io/Reader.html)