---
title:                "Oppretting av en midlertidig fil"
html_title:           "Java: Oppretting av en midlertidig fil"
simple_title:         "Oppretting av en midlertidig fil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Midlertidige filer er midlertidig opprettede filer som brukes av programmer for å lagre midlertidig informasjon. Dette kan være nyttig for å spare tid og ressurser ved å unngå å opprette permanente filer under utviklingsprosessen.

Det er flere grunner til at programmere bruker midlertidige filer. For det første kan det hjelpe med å feilsøke og teste kode, da det gir muligheten til å lagre og lese data på en effektiv måte. Det kan også være nyttig for å holde data innenfor en avgrenset og midlertidig kontekst, og hindre eventuelle krasj ved å skrive til permanente filer.

## Slik gjør du:

For å opprette en midlertidig fil i Java, kan du bruke File.createTempFile () metoden. Denne metoden tar inn to argumenter - prefix og suffix - for å definere navnet på den midlertidige filen. Når filen er opprettet, kan du lese og skrive data til filen som du vil med en vanlig fil.

Her er et eksempel på Java-kode som oppretter en midlertidig fil og skriver data til den:

```Java
File tempFile = File.createTempFile("midlertidig", ".txt");
// Skriver data til filen
FileWriter writer = new FileWriter(tempFile);
writer.write("Dette er en midlertidig fil.");
// Lukker filen
writer.close();
```
Etter at koden kjører, vil det bli opprettet en fil med navnet "midlertidig" og .txt som filtype, og teksten "Dette er en midlertidig fil" blir skrevet til filen.

## Dypdykk:

I tidligere versjoner av Java, var det nødvendig å spesifisere en mappe hvor midlertidige filer skulle lagres. Dette er ikke lenger tilfellet med nyere versjoner av Java, da det nå brukes operativsystemets midlertidige mappe som standard.

Det er også alternative måter å opprette midlertidige filer på i Java, som ved hjelp av RandomAccessFile-klassen eller ved å kalle File.createTempFile () metoden med et tredje argument for å spesifisere en bestemt filbane.

Implementeringen av midlertidige filer vil variere fra system til system, så det kan være lurt å teste og optimalisere din kode for å sikre en effektiv og pålitelig løsning.

## Se også:

Mer informasjon om bruk av midlertidige filer i Java kan du finne på følgende lenker:

- [Java Documentation: File Class](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java Tutorials: Temporary File I/O](https://docs.oracle.com/javase/tutorial/essential/io/tmpfile.html)