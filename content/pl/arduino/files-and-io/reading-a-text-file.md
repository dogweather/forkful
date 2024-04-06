---
date: 2024-01-20 17:53:41.607760-07:00
description: "How to: (Jak to zrobi\u0107:) Czytanie plik\xF3w tekstowych z karty\
  \ SD w Arduino zacz\u0119\u0142o si\u0119, gdy modu\u0142y SD sta\u0142y si\u0119\
  \ dost\u0119pne. Biblioteka SD u\u017Cywa protoko\u0142u SPI\u2026"
lastmod: '2024-04-05T22:50:50.021026-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Czytanie plik\xF3w tekstowych z karty SD w Arduino\
  \ zacz\u0119\u0142o si\u0119, gdy modu\u0142y SD sta\u0142y si\u0119 dost\u0119\
  pne."
title: Odczytywanie pliku tekstowego
weight: 22
---

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
