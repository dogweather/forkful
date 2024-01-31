---
title:                "Praca z plikami CSV"
date:                  2024-01-19
html_title:           "Bash: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"

category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
CSV to format pliku służący do przechowywania danych w formie tabelarycznej. Programiści używają go, by ułatwić wymianę danych między różnymi programami lub urządzeniami, dzięki jego prostocie i wszechstronności.

## Jak to zrobić:
Praca z CSV na Arduino zwykle obejmuje odczyt lub zapis danych przy użyciu karty SD. Oto przykład odczytu:

```Arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(10)) {
    Serial.println("Inicjalizacja karty SD nieudana!");
    return;
  }
  myFile = SD.open("dane.csv");

  if (myFile) {
    while (myFile.available()) {
      String data = myFile.readStringUntil('\n');
      Serial.println(data);
    }
    myFile.close();
  } else {
    Serial.println("Otwarcie pliku nieudane.");
  }
}

void loop() {
  // Nie potrzebujemy nic w pętli.
}
```

Przykład zapisu danych do pliku CSV:

```Arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(10)) {
    Serial.println("Inicjalizacja karty SD nieudana!");
    return;
  }

  myFile = SD.open("dane.csv", FILE_WRITE);
  if (myFile) {
    myFile.println("temperatura, wilgotnosc");
    myFile.println("23.5, 60");
    myFile.println("24.0, 58");
    myFile.close();
    Serial.println("Dane zapisane.");
  } else {
    Serial.println("Otwarcie pliku nieudane.");
  }
}

void loop() {
  // Nie potrzebujemy nic w pętli.
}
```

## Dogłębniej:
CSV pojawił się w latach 70-tych XX wieku. Jest proste: wartości oddzielone są przecinkami, a każdy wiersz to rekord danych. Alternatywą może być JSON czy XML, szczególnie gdy struktura danych jest bardziej złożona. Na poziomie implementacji pamiętaj, że Arduino ma ograniczone zasoby, więc trzymaj pliki małe i skoncentruj się na efektywnej manipulacji łańcuchami znaków.

## Zobacz również:
- Dokumentacja Arduino SD: https://www.arduino.cc/en/reference/SD
- Tutorial dotyczący pracy z CSV i Arduino: https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite
- Porównanie formatów danych (CSV, JSON, XML): https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite
