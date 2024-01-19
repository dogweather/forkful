---
title:                "Tworzenie tymczasowego pliku"
html_title:           "C#: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Tworzenie tymczasowego pliku polega na stworzeniu pliku, który jest używany tymczasowo i zazwyczaj usuwany po zakończeniu jego użycia. Programiści robią to, by przechować dane, które są potrzebne tylko przez krótki czas i nie ma potrzeby zapisywać ich trwale.

## Jak to zrobić:
Środowisko Arduino nie obsługuje bezpośrednio tworzenia plików tymczasowych, ale możemy to osiągnąć za pomocą karty SD i biblioteki SD. 

```Arduino
#include <SD.h>

File tempFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Nieudane inicjalizowanie karty SD");
    return;
  }
  
  tempFile = SD.open("temp.txt", FILE_WRITE);
  
  if (tempFile) {
    tempFile.println("To jest plik tymczasowy");
    tempFile.close();
    Serial.println("Plik tymczasowy został stworzony");
  }
  else {
    Serial.println("Błąd przy tworzeniu pliku tymczasowego");
  }
}

void loop() {
  // Nie robimy nic w pętli głównej
}
```

Po uruchomieniu kodu, powinieneś zobaczyć na monitorze szeregówkowym informację o utworzeniu pliku.

## Dogłębne spojrzenie:
Nie ma konkretnej instancji w historii, gdy pojawiło się tworzenie plików tymczasowych w Arduino. Jest to koncept, który pochodzi z bardziej zaawansowanych systemów operacyjnych, ale nadal może być użyteczny w Arduino ze względu na ograniczone zasoby. 

Alternatywą dla tworzenia tymczasowego pliku na karcie SD mogłoby być użycie EEPROM do przechowywania tymczasowych danych, chociaż jest ona bardziej odpowiednia do przechowywania małej ilości danych, które muszą przetrwać reset.

Kiedy tworzymy plik tymczasowy na karcie SD, dane są zapisywane na karcie jako bloki, co oznacza, że nawet małe zmiany mogą spowodować zapisanie całego bloku.

## Zobacz też:
- [Biblioteka SD w Arduino](https://www.arduino.cc/en/reference/SD)
- [Przechowywanie danych na karcie SD](https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite)
- [Arduino - EEPROM](https://www.arduino.cc/en/Reference/EEPROM)