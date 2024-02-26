---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:59.003271-07:00
description: "Praca z plikami CSV (Comma-Separated Values - warto\u015Bci oddzielone\
  \ przecinkami) w Arduino wi\u0105\u017Ce si\u0119 z odczytem z plik\xF3w CSV i zapisywaniem\
  \ do nich,\u2026"
lastmod: '2024-02-25T18:49:34.065726-07:00'
model: gpt-4-0125-preview
summary: "Praca z plikami CSV (Comma-Separated Values - warto\u015Bci oddzielone przecinkami)\
  \ w Arduino wi\u0105\u017Ce si\u0119 z odczytem z plik\xF3w CSV i zapisywaniem do\
  \ nich,\u2026"
title: Praca z plikami CSV
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z plikami CSV (Comma-Separated Values - wartości oddzielone przecinkami) w Arduino wiąże się z odczytem z plików CSV i zapisywaniem do nich, zazwyczaj przechowywanych na karcie SD, co umożliwia rejestrowanie danych, ustawienia konfiguracyjne i wiele więcej. Programiści często obsługują pliki CSV do zbierania danych z czujników, przechowywania parametrów konfiguracyjnych lub interfejsów z innymi systemami, ze względu na jego prostotę i szerokie przyjęcie na różnych platformach.

## Jak to zrobić:
Arduino nie posiada wbudowanej biblioteki specjalnie do obsługi plików CSV, ale możesz użyć bibliotek `SD` i `SPI` do dostępu do plików na karcie SD, a następnie przetwarzać lub generować dane CSV za pomocą podstawowych technik manipulacji ciągami znaków. Kiedy zajmujesz się bardziej złożoną manipulacją plikami CSV, biblioteka firm trzecich `ArduinoCSV` może być wykorzystana do łatwiejszego parsowania i zapisywania.

**Odczytywanie danych CSV z karty SD:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Inicjalizacja nie powiodła się!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      Serial.println(dataLine); // Wyświetla linię CSV
    }
    dataFile.close();
  } else {
    Serial.println("Błąd otwierania data.csv");
  }
}

void loop() {
  // Nie używane w tym przykładzie
}
```
*Przykładowe wyjście:*
```
SensorID, Timestamp, Value
1, 1597840923, 23.5
2, 1597840987, 22.4
```

**Zapisywanie danych CSV na karcie SD:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Inicjalizacja nie powiodła się!");
    return;
  }
  File dataFile = SD.open("output.csv", FILE_WRITE);
  if (dataFile) {
    dataFile.println("SensorID, Timestamp, Value"); // Nagłówek CSV
    dataFile.println("1, 1597840923, 23.5"); // Przykładowy wiersz danych
    dataFile.close();
    Serial.println("Dane zapisane");
  } else {
    Serial.println("Błąd otwierania output.csv");
  }
}

void loop() {
  // Nie używane w tym przykładzie
}
```
*Przykładowe wyjście:*
```
Dane zapisane
```

**Użycie ArduinoCSV do parsowania:**
Jeśli zajmujesz się skomplikowanymi plikami CSV, biblioteka `ArduinoCSV` może znacznie uproszczyć prace związane z parsowaniem. Ten przykład zakłada, że zainstalowałeś już bibliotekę `ArduinoCSV`.

```cpp
#include <SPI.h>
#include <SD.h>
#include <ArduinoCSV.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Inicjalizacja nie powiodła się!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    CSVParser parser;
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      if (parser.parseLine(dataLine)) {
        for (int i = 0; i < parser.count(); i++) {
          Serial.print(parser.getField(i)); // Wyświetl każde pole
          if (i < parser.count() - 1) {
            Serial.print(", ");
          }
        }
        Serial.println();
      }
    }
    dataFile.close();
  } else {
    Serial.println("Błąd otwierania data.csv");
  }
}

void loop() {
  // Nie używane w tym przykładzie
}
```
*Przykładowe wyjście:*
```
SensorID,  Timestamp,  Value
1,  1597840923,  23.5
2,  1597840987,  22.4
```
W tych przykładach, poprzez odczytywanie z plików CSV i zapisywanie do nich na karcie SD, projekty Arduino mogą łatwo zbierać dane, przechowywać ustawienia konfiguracyjne lub wymieniać dane z innymi aplikacjami w uniwersalnie dostępnym formacie.
