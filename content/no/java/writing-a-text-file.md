---
title:    "Java: Å skrive en tekstfil"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Å skrive en tekstfil kan være en nyttig ferdighet å ha for programmerere av alle nivåer. Enten du ønsker å lagre data, lage konfigurasjonsfiler eller bare øve på å skrive og lese filer, kan å kunne skrive en tekstfil være en svært nyttig ferdighet å ha.

## Slik gjør du det
For å kunne skrive en tekstfil i Java, må du først opprette en File-objekt og en FileWriter-objekt. Deretter kan du bruke metoden write() på FileWriter-objektet for å skrive tekst til filen. Husk å lukke FileWriter-objektet når du er ferdig med å skrive til filen.

```Java
// Oppretter en File-objekt
File file = new File("minfil.txt");
// Oppretter en FileWriter-objekt og gir filnavnet som parameter
FileWriter fw = new FileWriter(file);
// Skriver tekst til filen
fw.write("Dette er en tekstfil skrevet i Java.");
// Lukker FileWriter-objektet for å fullføre skrivingen
fw.close();
```

Nå har du opprettet og skrevet til en tekstfil ved hjelp av Java!

## Dypdykk
Det finnes også flere måter å skrive tekst til en fil på i Java, som for eksempel ved hjelp av BufferedWriter eller PrintWriter. Disse klassene gir mulighet for å skrive formatert tekst til filen og gir også ekstra funksjonalitet som å skrive til nye linjer eller å lese fra tastaturet.

Det er også viktig å huske på å håndtere eventuelle exceptions som kan oppstå når du skriver til eller leser fra en fil. All kode som involverer å skrive til fil bør plasseres i en try-catch-blokk for å sikre at programmet ikke stopper dersom det skulle oppstå en feil.

## Se også
- [Java Official Documentation on File I/O](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)
- [Write and Read Text Files in Java](https://www.baeldung.com/java-write-to-file)
- [Java File I/O Tutorial](https://www.tutorialspoint.com/java/io/java_io_filewriter.htm)