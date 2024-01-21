---
title:                "Odczytywanie pliku tekstowego"
date:                  2024-01-20T17:53:41.607760-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Czytanie pliku tekstowego to proces pobierania danych z pliku zapisanego na karcie SD lub pamięci systemu. Programiści robią to, by załadować konfiguracje, dane wejściowe lub po prostu wyświetlić informacje na ekran.

## How to: (Jak to zrobić:)
```Arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  
  // Sprawdź, czy karta SD jest dostępna
  if (!SD.begin(4)) {
    Serial.println("Blad karty SD");
    return;
  }
  
  // Otwórz plik w trybie czytania
  myFile = SD.open("test.txt");
  if (myFile) {
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close(); // Zawsze pamiętaj, by zamknąć plik po skończeniu.
  } else {
    Serial.println("Błąd otwarcia pliku");
  }
}

void loop() {
  // Nic nie rób.
}

```
Sample output:
```
Witaj, Arduino!
Dane konfiguracyjne: 123
```

## Deep Dive (Dogłębna analiza)
Czytanie plików tekstowych z karty SD w Arduino zaczęło się, gdy moduły SD stały się dostępne. Biblioteka SD używa protokołu SPI i pozwala zarówno na odczyt, jak i zapis plików. Istnieje kilka alternatyw, jak używanie pamięci EEPROM w Arduino, ale karty SD oferują więcej miejsca i są wygodniejsze w użyciu. W implementacji ważne jest prawidłowe zamontowanie karty SD i obsługa błędów, by zapewnić stabilność wykonywanego programu.

## See Also (Zobacz również)
- [Arduino - File Read](https://www.arduino.cc/en/Reference/FileRead)
- [Arduino - SD Library](https://www.arduino.cc/en/reference/SD)
- [SPI Communication in Arduino](https://www.arduino.cc/en/reference/SPI)