---
title:                "Å skrive en tekstfil"
html_title:           "Java: Å skrive en tekstfil"
simple_title:         "Å skrive en tekstfil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Skal du skrive et Java-program som håndterer store mengder data? Da kan det være nyttig å kunne skrive dataen din til en tekstfil. Det gjør at du kan lagre dataen og lese den senere, og det kan også være enklere å behandle store datamengder på den måten.

## Slik gjør du det

For å skrive en tekstfil i Java, må du følge noen enkle steg. Først må du opprette et nytt filobjekt, som vil representere filen du ønsker å skrive til.

```Java
File file = new File("minfil.txt");
```

Deretter må du åpne en FileWriter og koble den til filobjektet.

```Java
FileWriter writer = new FileWriter(file);
```

For å skrive data til tekstfilen, må du bruke FileWriter-objektets write() metode.

```Java
writer.write("Dette er en testtekst.");
```

Husk å lukke FileWriter-objektet når du er ferdig.

```Java
writer.close();
```

## Dypdykk

Nå som du har lært hvordan du skriver data til en tekstfil i Java, kan det være nyttig å vite noen flere detaljer. Det første du bør vite er at FileWriter-klassen bruker en buffer for å skrive data til filen, som betyr at dataen ikke blir lagret umiddelbart. For å sikre at dataen faktisk blir lagret, kan du enten bruke FileWriter-objektets flush() metode eller methoden close().

En annen ting å være obs på er at FileWriter vil overskrive eksisterende data i filen hver gang du åpner den på nytt. Hvis du ønsker å legge til data i filen i stedet for å overskrive, kan du bruke FileWriter-objektets constructor med en boolean parameter som indikerer dette.

```Java
FileWriter writer = new FileWriter(file, true);
```

Et siste tips er å bruke try-with-resources syntaksen når du åpner en fil for å sørge for at ressursene blir lukket automatisk.

```Java
try (FileWriter writer = new FileWriter(file)) {
    writer.write("Dette er en testtekst.");
} catch (IOException e) {
    e.printStackTrace();
}
```

## Se også

- Java FileWriter dokumentasjon: https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html
- Java File dokumentasjon: https://docs.oracle.com/javase/7/docs/api/java/io/File.html
- Tutorialspoint sin guide til å skrive til en tekstfil i Java: https://www.tutorialspoint.com/javaexamples/file_write.htm