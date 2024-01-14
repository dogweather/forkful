---
title:    "Arduino: Opprette en midlertidig fil"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##Hvorfor
Å lage midlertidige filer kan være en nyttig teknikk når du koder med Arduino. Det kan hjelpe deg med å organisere og lagre data i løpet av programkjøringen.

##Slik gjør du det
For å lage en midlertidig fil i Arduino, kan du bruke funksjonen "tempFile = File.createTempFile("navn", "txt");". Dette vil opprette en fil med navnet "navn" og legge til ".txt" som filtype. Du kan også legge til et tredje argument for å spesifisere plasseringen for filen.

```
Arduino
File tempFile = File.createTempFile("sensorData", "csv");
```

Når filen er opprettet, kan du bruke "tempFile.print()" for å skrive data til filen og "tempFile.read()" for å lese data fra filen.

```
Arduino
void setup() {
  File tempFile = File.createTempFile("sensorData", "csv");
  
  // Skriv data til filen
  tempFile.print("Temperatur, Fuktighet\n");
  tempFile.print("25.5, 60%\n");
  tempFile.print("26.8, 55%\n");
  tempFile.print("24.7, 65%\n");

  // Les data fra filen
  while (tempFile.available()) {
    Serial.println(tempFile.read());
  }

  // Lukk filen
  tempFile.close();
}
```

##Dypdykk
Når du lager midlertidige filer i Arduino, blir de automatisk slettet når programmet avsluttes. Dette er nyttig for å spare på minne og unngå rot med flere filer.

Du kan også angi størrelsen på den midlertidige filen ved å legge til et fjerde argument i "File.createTempFile" funksjonen. Dette er nyttig hvis du forventer at filen vil inneholde mye data, og du vil unngå å bruke for mye minne.

##Se også
- [Offisiell dokumentasjon om File.createTempFile](https://www.arduino.cc/reference/en/libraries/arduino-littlefs/using-file-create-temp-file/)
- [Mer informasjon om midlertidige filer i Arduino](https://forum.arduino.cc/t/tempfile-no-example-code-in-this-forum-is-working-needs-correction-or-explanation/704108)