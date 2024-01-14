---
title:    "Arduino: Å skrive en tekstfil"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil kan være en nyttig ferdighet for Arduino-programmering, spesielt når det gjelder å lagre data og persistens. Det kan også være nyttig for debugging formål.

## Hvordan

For å skrive en tekstfil, må du følge disse trinnene:

```Arduino
File file = SD.open("tekstfil.txt", FILE_WRITE); // Åpne filen for skriving
if (file) {
  file.println("Dette er en tekst som vil bli skrevet til tekstfilen."); // Skriv tekst til filen
  file.close(); // Lukk filen
}
```

For å lese innholdet i filen, kan du bruke følgende kode:

```Arduino
File file = SD.open("tekstfil.txt"); // Åpne filen for lesing
if (file) {
  while (file.available()) { // Så lenge det er tilgjengelige tegn i filen
    Serial.println(file.readStringUntil('\n')); // Les en linje og skriv ut den til seriell monitor
  }
  file.close(); // Lukk filen
}
```

Output vil være:

```
Dette er en tekst som vil bli skrevet til tekstfilen.
```

## Deep Dive

Når du skriver en tekstfil, kan du også spesifisere hvilken modus du vil åpne filen i. I eksemplet ovenfor brukte vi `FILE_WRITE`, men du kan også bruke `FILE_READ` for å lese filen eller `FILE_APPEND` for å legge til nytt innhold uten å overskrive det som allerede finnes i filen.

Det er også viktig å lukke filen når du er ferdig med å lese eller skrive for å sikre at alle data blir lagret. Hvis du ikke gjør dette, kan filen bli korrupt.

## Se også

- [Arduino SD Library](https://www.arduino.cc/en/Reference/SD)
- [How to Read and Write Files on an SD Card with an Arduino](https://www.circuitspecialists.com/blog/how-to-read-and-write-files-on-an-sd-card-with-an-arduino/)
- [File Handling with Arduino](https://www.arduino.cc/en/Tutorial/FileWrite)