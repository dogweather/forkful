---
title:                "Opprettelse av en midlertidig fil"
html_title:           "Arduino: Opprettelse av en midlertidig fil"
simple_title:         "Opprettelse av en midlertidig fil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Noen ganger kan det være nyttig å opprette en midlertidig fil mens man programmerer Arduino. Dette gjør det mulig å lagre data midlertidig, for eksempel under sensormålinger eller for å lagre mellomresultater i et program.

## Hvordan
For å opprette en midlertidig fil på Arduino, kan du bruke følgende kode:

```
Arduino void setup() {
  // Åpne en midlertidig fil med filnavnet "tempfile"
  File tmpfile = SD.open("tempfile", FILE_WRITE);
  // Skriv inn data i filen
  tmpfile.write("Dette er en midlertidig fil!");
  // Lukk filen
  tmpfile.close();
}
```

Når du kjører koden, vil du se en midlertidig fil bli opprettet på SD-kortet (hvis du bruker ett). Hvis du åpner filen, vil du se teksten som ble skrevet inn i filen.

## Dykk dypere
Denne teknikken kan også være nyttig for å lagre mellomresultater eller andre data underveis i et program. For å få tilgang til dataene senere, kan du bruke samme kode som over, men med et nytt filnavn hver gang programmet kjører.

En annen metode for å opprette en midlertidig fil er å bruke ```tmpfile = File.temporary()```. Dette vil opprette en fil med et unikt og tilfeldig filnavn hver gang den kjøres.

## Se også
- [SD-kortbibliotek for Arduino](https://www.arduino.cc/en/Reference/SD)
- [Fil.h bibliotek for Arduino](https://www.arduino.cc/en/Reference/SDFile)