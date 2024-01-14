---
title:    "Arduino: Lage en midlertidig fil"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Hvorfor
Å opprette en midlertidig fil i din Arduino-programmering kan være nyttig av flere årsaker. Det kan hjelpe deg med å lagre data midlertidig og gi deg mer kontroll over hvordan dataene dine blir behandlet.

## Hvordan
Arduino har en innebygd funksjon, `tmpfile()`, som lar deg opprette midlertidige filer på enheten din. Her er et enkelt eksempel på hvordan du kan bruke denne funksjonen:

```Arduino
File tempFile = tmpfile(); // Oppretter en midlertidig fil og lagrer den i variabelen "tempFile"
if (tempFile) { // Sjekker om filen ble opprettet riktig
  Serial.println("Midlertidig fil opprettet!"); // Skriver ut en melding til seriell monitor
  fprintf(tempFile, "Dette er en midlertidig fil"); // Skriver tekst til den midlertidige filen
  fclose(tempFile); // Lukker den midlertidige filen
}
```

Nå når du har opprettet en midlertidig fil, kan du bruke den som du vil i koden din. Husk å slette den når du er ferdig med å bruke den, slik at den ikke tar opp unødvendig plass på enheten din.

> **Merk:** `tmpfile()`-funksjonen lager bare en midlertidig fil i RAM, og den vil bli slettet når enheten din blir slått av eller tilbakestilt.

## Dypdykk
Hvis du ønsker mer kontroll over hvordan filen din blir opprettet og slettet, kan du bruke `tmpnam()`-funksjonen. Denne funksjonen lar deg opprette en midlertidig fil i et annet område enn bare RAM. Her er et eksempel på hvordan du kan bruke `tmpnam()`, og deretter slette den midlertidige filen manuelt:

```Arduino
char name[L_tmpnam]; // Lager et array for å lagre navnet på den midlertidige filen
tmpnam(name); // Oppretter den midlertidige filen og lagrer navnet i arrayet
File tempFile = SD.open(name, FILE_WRITE); // Åpner den midlertidige filen for skriving
if (tempFile) { // Sjekker om filen ble opprettet riktig
  Serial.print("Midlertidig fil opprettet med navn: ");
  Serial.println(name); // Skriver ut navnet på den midlertidige filen
  tempFile.print("Dette er en midlertidig fil"); // Skriver tekst til den midlertidige filen
  tempFile.close(); // Lukker den midlertidige filen
  SD.remove(name); // Sletter den midlertidige filen
}
```

Nå kan du kontrollere hvor filen blir opprettet og slettet, i tillegg til å kunne bruke den som en midlertidig lagringsplass for dataene dine.

## Se også
- Arduino-funksjoner som brukes i denne bloggposten:
  - [tmpfile()](https://www.arduino.cc/reference/en/language/functions/communication/tmpfile/)
  - [tmpnam()](https://www.arduino.cc/reference/en/language/functions/communication/tmpnam/)
- [SD-kortmodulen for Arduino](https://www.arduino.cc/en/Reference/SD)
- [Hvordan lagre data i Arduino](https://blog.arduino.cc/2017/11/27/how-to-save-data-in-arduino/)