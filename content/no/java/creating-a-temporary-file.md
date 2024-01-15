---
title:                "Opprette en midlertidig fil"
html_title:           "Java: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Noen ganger når vi jobber med større programmer eller har behov for midlertidig lagring av data, kan det være nyttig å opprette en midlertidig fil i Java. Denne artikkelen vil lære deg hvordan du kan opprette og håndtere midlertidige filer i Java.

## Hvordan
For å opprette en midlertidig fil i Java, kan du bruke klassen `File` og dens metode `createTempFile()`. Her er et eksempel på hvordan du kan gjøre det:

```Java
File tempFile = File.createTempFile("myTempFile", ".txt");
```

Dette oppretter en midlertidig fil med navnet "myTempFile[tilfeldig tall].txt", der "[tilfeldig tall]" er et unikt tall som blir generert for å sikre at filnavnet er unikt. 

Du kan også spesifisere en bestemt plassering for å opprette den midlertidige filen, for eksempel:

```Java
File tempFile = File.createTempFile("myTempFile", ".txt", new File("C:/temp"));
```

Dette vil opprette filen "myTempFile[tilfeldig tall].txt" i mappen "C:/temp".

Når du er ferdig med å bruke den midlertidige filen, kan du slette den ved å bruke metoden `delete()`:

```Java
tempFile.delete();
```

## Deep Dive
Når du oppretter en midlertidig fil i Java, er det viktig å vite at denne filen vil bli slettet så snart Java-programmet avsluttes eller når den midlertidige filen blir eksplisitt slettet ved hjelp av `delete()`-metoden. Dette er noe du må være klar over når du håndterer data som skal lagres midlertidig.

Du kan også spesifisere en prefiks og suffiks på filnavnet ved å bruke metoden `createTempFile(String prefix, String suffix)`. Dette kan være nyttig hvis du vil ha bedre kontroll over filnavnet. 

En annen viktig ting å huske på er at midlertidige filer blir opprettet som skjulte filer, så hvis du vil se dem i filutforskeren må du aktivere muligheten for å vise skjulte filer.

## Se også
- [Offisiell Java API dokumentasjon for File-klasse](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Tutorialspoint: Working with Temporary Files in Java](https://www.tutorialspoint.com/javaexamples/java_files.htm)