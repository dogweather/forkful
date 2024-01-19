---
title:                "Opprette en midlertidig fil"
html_title:           "C#: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Å Lage Midlertidige Filer i Java

## Hva og Hvorfor?
Å lage en midlertidig fil er en prosess der filer opprettes for kortvarig lagring og datahåndtering. Dette er praktisk for å lagre store datamengder som krever behandling over tid, uten å bruke hovedlageret.

## Hvordan:
Lag en midlertidig fil ved hjelp av Java’s File-klassen:
```Java
import java.io.File;
import java.io.IOException;

public class TempFileExample {
    public static void main(String[] args) {
        try {
            File tempFile = File.createTempFile("tempFileExample", ".txt");
            System.out.println("Temp file: " + tempFile.getAbsolutePath());
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
}
```
Output:
```
Temp file: C:\Users\Username\AppData\Local\Temp\tempFileExample8468678674557948574.txt
```

## Dyp Dykk
Historisk sett har midlertidig filhåndtering eksistert siden tidlige operativsystemer. Selv om bruk av dem kan være mindre vanlig nå med større minnekapasiteter, forblir deres effektivitet uerstattelig i enkelte tilfeller. 

I Java 2.0 ble klassen `java.io.File` introdusert for å håndtere filsystemoppgaver, inkludert oppretting av midlertidige filer. Alternative metoder inkluderer bruk av tredjepartsbiblioteker som Apache Commons IO.

Implementeringsdetaljer involverer spesifisering av prefikset og suffikset for den midlertidige filen ved å sende de som argumenter til `createTempFile`-metoden. Denne metoden skaper så en unik, tom fil i standard midlertidig filkatalog med de gitte prefikset og suffikset.

## Se Også
1. [Oracle Java Documentation on File](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
2. [Apache Commons IO](https://commons.apache.org/proper/commons-io/)
3. [Java Tutorial for File Handling](https://www.javatpoint.com/java-file)