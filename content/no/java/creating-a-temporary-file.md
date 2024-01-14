---
title:                "Java: Å opprette en midlertidig fil"
simple_title:         "Å opprette en midlertidig fil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Oppretting av midlertidige filer er en vanlig oppgave i Java-programmering. Dette kan være nyttig når man trenger å lagre midlertidige data eller behandle store mengder data som ikke trenger å lagres permanent. Midlertidige filer kan også være nyttige for testing, debugging og logging formål.

## Hvordan
Oppretting av en midlertidig fil i Java er en enkel prosess. Det første du må gjøre er å importere "java.io.File" og "java.io.IOException" bibliotekene. Deretter kan du bruke "createTempFile()" metoden i File-klassen for å opprette en midlertidig fil. Her er et eksempel på kode som kan brukes for å opprette en midlertidig fil med navnet "temp.txt" i rotmappen:

```Java
import java.io.File;
import java.io.IOException;

public class Main {
    public static void main(String[] args) throws IOException {
        File tempFile = File.createTempFile("temp", ".txt");
        System.out.println("Midlertidig fil opprettet på: " + tempFile.getAbsolutePath());
    }
}
```

Dette vil skrive ut banen til den nyopprettede midlertidige filen. Det er viktig å merke seg at filen vil automatisk bli slettet når programmet avsluttes.

## Deep Dive
For å forstå mer om opprettelse av midlertidige filer, er det viktig å vite at det er to typer midlertidige filer i Java: hardlink-filer og slettede filer. Hardlink-filer opprettes ved å kopiere eksisterende filer, mens slettede filer opprettes ved å skrive til en ny fil. Hardlink-filer har samme inode-nummer som den originale filen og tar derfor opp mindre plass på disken.

En annen viktig ting å merke seg er at midlertidige filer kan opprettes på et bestemt sted ved å bruke "createTempFile(String prefix, String suffix, File directory)" metoden. Dette kan være nyttig hvis du vil ha kontroll over hvor midlertidige filer skal lagres.

## Se også
- [Java Documentation for File class](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-java.io.File-)
- [Tutorialspoint article on creating temporary files in Java](https://www.tutorialspoint.com/java/io/java_io_file_createtempfile.htm)
- [Stack Overflow thread on the difference between hardlink and deleted temporary files](https://stackoverflow.com/questions/16458147/what-is-the-difference-between-hard-link-and-temporary-file)

For mer informasjon om opprettelse av midlertidige filer i Java, sjekk ut disse ressursene. Til neste gang, happy coding!