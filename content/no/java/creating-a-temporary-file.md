---
title:                "Java: Opprette en midlertidig fil"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Hvorfor

Skal du begynne å lage midlertidige filer i Java eller kjenner du noen som kunne trenge å vite mer om det? Da er denne bloggposten for deg! Her vil vi diskutere hvorfor noen ville ønske å lage midlertidige filer og hvordan man gjør det.

# Hvordan

Lage midlertidige filer i Java er enkelt ved hjelp av "File.createTempFile()" metoden. Denne metoden oppretter en ny midlertidig fil i systemets standard midlertidige filkatalog. La oss se på et eksempel:

```Java
File tempFile = File.createTempFile("temp", ".txt");
System.out.println(tempFile.getAbsolutePath());
```

Output:

```
C:\Users\Bruker\AppData\Local\Temp\temp2274415280275047538.txt
```

I dette eksempelet blir en midlertidig fil med prefikset "temp" og filtypen ".txt" opprettet i systemets midlertidige filkatalog. Metoden "createTempFile()" tar også inn en annen parameter som lar deg spesifisere en annen midlertidig filkatalog. Dette kan for eksempel være nyttig hvis du trenger å lagre de midlertidige filene på en spesifikk plassering.

Det er viktig å huske på at midlertidige filer ikke vil bli slettet automatisk når programmet avsluttes. Det er ditt ansvar å slette dem når de ikke lenger er nødvendige. Dette kan enkelt gjøres ved hjelp av "File.delete()" metoden.

# Dypdykk

Det er verdt å nevne at midlertidige filer også kan brukes til å lagre og utveksle data mellom ulike deler av et program eller mellom forskjellige programmer. For eksempel kan en midlertidig fil brukes som en buffer for å lagre data som skal behandles senere. Dette kan være nyttig i situasjoner der man må vente på eksterne prosesser eller brukerinput.

En annen ting å merke seg er at midlertidige filer kan ha forskjellige sikkerhetskontroller avhengig av operativsystemet og filkatalogens tillatelser. Det er derfor viktig å ha riktig tilgang og håndtere eventuelle unntak som kan oppstå.

# Se også

- [Offisiell Java dokumentasjon for File.createTempFile()](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-java.io.File-)
- [Mer om Java File API](https://www.baeldung.com/java-file)
- [Om midlertidige filer på Windows og Linux](https://stackoverflow.com/questions/41314031/java-tempgfile-platform-specific-path)