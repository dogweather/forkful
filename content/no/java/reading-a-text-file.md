---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Lesing av en tekstfil i Java

## Hva & Hvorfor?
Lesing av en tekstfil betyr å hente inn data fra filen for å bruke det i programmet ditt. Det hjelper til med å lagre dataene permanent og gir muligheten til å arbeide med større sett av data som kan ligge utenfor selve programmet.

## Hvordan
Her er en grunnleggende kode for å lese en tekstfil i Java ved hjelp av `FileReader` og `BufferedReader`.

```Java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main {

    public static void main(String[] args) {
  		
        try {
            FileReader reader = new FileReader("test.txt");
            BufferedReader bufferedReader = new BufferedReader(reader);
 
            String line;
 
            while ((line = bufferedReader.readLine()) != null) {
                System.out.println(line);
            }
            reader.close();
 
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
Når du kjører denne koden, vil Java lese "test.txt" og skrive ut innholdet til konsollen.

## Dypere dykk
Historisk sett har Java alltid hatt funksjonaliteten til å lese tekstfiler, men i nyere versjoner har de gjort begrepet mer robust med innføringen av `java.nio.file` pakken. Alternativt kan du også bruke `Scanner` klassen for å lese en tekstfil i Java, noe som kan være mer bekvemt for enkelte brukere ettersom den har metoder for enklere parsing av primitive datatyper. Implementeringsdetaljene for å lese en tekstfil kan være forskjellige avhengig av bestemte krav, som størrelsen på filen og hvordan dataene skal brukes. 

## Se også
For mer informasjon om hvordan lese tekstfiler i Java, sjekk ut disse nyttige lenkene:

- [Java FileReader Class](https://www.w3schools.com/java/java_files_read.asp)
- [Reading and Writing Files in Java](https://www.baeldung.com/java-io)
- [Java BufferedReader Class](https://www.javatpoint.com/java-bufferedreader-class)