---
title:    "Arduino: Lese en tekstfil"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en Arduino-entusiast og er interessert i å lese tekstfiler, så er dette blogginnlegget perfekt for deg. Her vil vi gå gjennom hvordan du enkelt kan lese tekstfiler ved hjelp av Arduino og gi deg en grundig forståelse av dette konseptet.

## Hvordan

For å lese en tekstfil ved hjelp av Arduino, må du bruke funksjonene "open" og "read" i SD-biblioteket. Først må du initialisere SD-kortet ved hjelp av funksjonen "begin", og deretter åpner du filen ved hjelp av "open" funksjonen. Deretter kan du bruke "read" funksjonen til å lese hver linje av teksten og lagre den i en variabel.

```Arduino
#include <SD.h>

void setup()
{
  Serial.begin(9600); // Initialiserer seriell kommunikasjon
  Serial.print("Initializing SD card...");
  
  if(!SD.begin(10)) // Initialiserer SD-kortet på pin 10
  {
    Serial.println("Initialization failed!");
    return;
  }
  
  Serial.println("Initialization done.");

  File myFile = SD.open("tekstfil.txt"); // Åpner tekstfilen "tekstfil.txt"

  if(myFile) // Sjekker om filen er åpen
  {
    while(myFile.available()) // Leser hver linje i filen
    {
      Serial.write(myFile.read()); // Skriver ut linjen til seriell monitor
    }
    myFile.close(); // Lukker filen
  }

  else // Hvis filen ikke åpnet
  {
    Serial.println("Error opening file!");
  }
}

void loop()
{
  // Ingenting her
}
```

Når du kjører koden og åpner seriell monitoren, vil du kunne se innholdet i tekstfilen "tekstfil.txt" skrevet ut.

## Deep Dive

Når du leser en tekstfil ved hjelp av Arduino, må du vite at hver linje i teksten blir lagret som en streng. Derfor må du bruke funksjonen "readString" hvis du ønsker å lese en hel linje om gangen. Du kan også bruke andre funksjoner som "readInt" eller "readLong" for å lese tall fra teksten.

Det er også viktig å merke seg at når du leser en tekstfil, starter filpekeren på begynnelsen av filen for hver iterasjon. Dette betyr at hvis du vil lese en bestemt del av filen, må du først bruke funksjonen "seek" for å flytte filpekeren til riktig posisjon.

## Se også

- [Arduino SD bibliotek](https://www.arduino.cc/en/Reference/SD)
- [Eksempelkode for å lese tekstfiler](https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadASCIIString)
- [Forbundet API for å lese tekstfiler](https://www.arduino.cc/en/Reference/API)