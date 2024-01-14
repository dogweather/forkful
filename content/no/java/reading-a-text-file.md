---
title:                "Java: Lesing av en tekstfil"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

I denne bloggposten skal vi se på hvordan man kan lese en tekstfil ved hjelp av Java-programmering. Å lese en tekstfil kan være nyttig for å hente data fra en ekstern fil, for eksempel en database eller en loggfil. Det kan også være nyttig for å automatisere visse oppgaver, som for eksempel å importere data til et annet program.

## Hvordan

For å lese en tekstfil i Java, må vi først opprette en File-objekt og knytte den til filen vi ønsker å lese. Dette gjør vi ved å bruke File-klassen og spesifisere filens navn og plassering som parameter.

```Java
File fil = new File("tekstfil.txt");
```

Deretter trenger vi en Scanner-objekt for å kunne lese innholdet i tekstfilen. Vi kan opprette dette objektet ved å bruke constructoren til Scanner-klassen og sende inn File-objektet vårt som parameter.

```Java
Scanner scanner = new Scanner(fil);
```

Nå kan vi bruke Scanner-objektet til å lese linje for linje i tekstfilen ved hjelp av en while-løkke. Vi kan også bruke hasNext() og next() metoder for å lese innholdet av filen til vi når slutten av filen.

```Java
while (scanner.hasNext()) {
  String linje = scanner.next();
  System.out.println(linje);
}
```

Det er også mulig å lese hele filen på en gang ved å bruke nextLine() metoden.

```Java
String allText = scanner.nextLine();
System.out.println(allText);
```

Etter at vi er ferdig med å lese filen, må vi lukke Scanner-objektet for å frigjøre ressurser.

```Java
scanner.close();
```

Etter at vi har lukket Scanner-objektet, kan vi jobbe med innholdet fra tekstfilen videre.

## Dypdykk

Det finnes flere metoder for å lese en tekstfil i Java, som for eksempel FileReader og BufferedReader. Disse kan være nyttige dersom man ønsker å lese en fil med spesielle egenskaper eller hvis man trenger å lese innholdet i filen på en spesifikk måte.

Det kan også være lurt å inkludere feilhåndtering når man leser en tekstfil, for å unngå uventede feil eller kræsj i programmet. Dette kan gjøres ved å bruke try-catch blokker eller ved å bruke throws-klausul i metoden som leser filen.

En annen måte å håndtere tekstfiler på er å bruke FileWriter og BufferedWriter for å skrive til filen. Dette kan være nyttig hvis man ønsker å legge til ny informasjon i en eksisterende fil, eller skrive ut data fra et program og lagre det i en fil.

## Se også

- [Filbehandling i Java](https://www.w3schools.com/java/java_files.asp)
- [Java Scanner-klassen](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html)
- [Java FileWriter og BufferedWriter](https://docs.oracle.com/javase/8/docs/api/java/io/FileWriter.html)