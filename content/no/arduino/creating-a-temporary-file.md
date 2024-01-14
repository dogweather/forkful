---
title:                "Arduino: Opprette en midlertidig fil"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lage midlertidige filer kan være en effektiv måte å organisere og lagre data på mens du kjører et Arduino program. Dette er spesielt nyttig hvis du trenger å behandle og manipulere store mengder data som kan overskride de begrensede minneressursene til en Arduino mikrokontroller.

## Hvordan lage en midlertidig fil på Arduino

For å lage en midlertidig fil på Arduino, må du først inkludere filsystembiblioteket i koden din ved å legge til følgende linje på toppen av filen:

```Arduino
#include <FS.h>
```

Deretter trenger du en variabel til å representere den midlertidige filen. Dette kan gjøres ved å bruke `File`-typen og tilordne den til `File::createTempFile()`-funksjonen:

```Arduino
File tempFile = File::createTempFile();
```

Nå som filen er laget, kan du skrive data til den ved hjelp av `print()` eller `println()`-funksjonene:

```Arduino
tempFile.println("Dette er en midlertidig fil!");
```

Når du er ferdig med å skrive til filen, må du lukke den ved hjelp av `close()`-funksjonen:

```Arduino
tempFile.close();
```

Du kan også lese data fra den midlertidige filen på samme måte som du leser data fra andre filer ved å bruke `read()`- eller `readString()`-funksjonene.

## Dypdykk

Når du lager en midlertidig fil på Arduino, lagres den i ESP32s interne flashminne. Dette minnet er delt inn i to seksjoner - en for data, og en for koden til programmet ditt. Det midlertidige filsystemet er plassert i dataområdet, noe som betyr at det vil ha en begrenset størrelse som vil variere avhengig av størrelsen på koden din.

Det er også viktig å merke seg at å lage midlertidige filer kan påvirke ytelsen til andre funksjoner på Arduinoen din. Dette skyldes at skriving og lesing fra filsystemet kan være en ressurskrevende operasjon.

## Se også

- [Offisiell ESP32 dokumentasjon om filsystemet](https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/storage/spiffs.html)
- [Tutorial om å bruke SPIFFS filsystemet på ESP32](https://randomnerdtutorials.com/esp32-spiffs-file-system-tutorial/)