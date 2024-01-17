---
title:                "Lesing av en tekstfil"
html_title:           "Arduino: Lesing av en tekstfil"
simple_title:         "Lesing av en tekstfil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

Hva og hvorfor?
Det å lese en tekstfil betyr å åpne en fil og lese informasjonen som er lagret i den. Dette er en viktig del av programmering fordi det lar deg hente og behandle data fra filer, noe som kan være nyttig for å håndtere store mengder informasjon eller for å samhandle med eksterne enheter.

Hvordan:
Du kan lese en tekstfil ved å bruke "File" funksjonen i Arduino biblioteket. Først må du åpne filen ved å oppgi filnavnet og "r"-modus for å lese. Deretter kan du lese informasjonen i fila ved å bruke "readString" eller "readLine" funksjonene. Her er et eksempel på hvordan dette kan gjøres:

```
Arduino fil = fil.open("data.txt", FILE_READ); // Åpner filen "data.txt"
hvis (fil) {                                   // Sjekker om åpningen var vellykket
  streng s = fil.readString();                // Leser hele filen som en streng
  fil.close();                                // Lukker filen for å spare minne
  seriell.print(s);                          // Sender informasjonen til seriell overvåking
}
```

Dykk ned:
Å lese tekstfiler har vært en viktig del av programmering siden de første datamaskinene ble laget. Det finnes også andre måter å lese filer på, for eksempel ved å bruke tekstbehandlingsprogrammer eller databaseteknologi. I Arduino, kan du også bruke SD-kortet for å lese eksterne filer. Det er viktig å lære om hvordan du leser tekstfiler for å kunne behandle store mengder informasjon og for å kommunisere med andre enheter, noe som er spesielt nyttig for Internet of Things (IoT) prosjekter.

Se også:
- Arduino Offisiell Dokumentasjon: https://www.arduino.cc/reference/en/language/functions/files-io/file/
- "Reading and Writing Files on an SD Card with an Arduino": https://learn.sparkfun.com/tutorials/reading-and-writing-files-to-an-sd-card-with-an-arduino/all