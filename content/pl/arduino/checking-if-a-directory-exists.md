---
title:                "Sprawdzanie, czy katalog istnieje"
date:                  2024-01-19
html_title:           "Bash: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"

category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Weryfikacja, czy katalog istnieje, to sprawdzenie funkcji systemu plików w poszukiwaniu folderu. Robimy to, aby uniknąć błędów przy próbie dostępu do nieistniejących folderów lub przed ich tworzeniem.

## Jak to zrobić:
```Arduino
#include <SD.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // czekaj na otwarcie połączenia szeregowego
  }
  if (!SD.begin()) {
    Serial.println("Inicjalizacja karty SD nie powiodła się.");
    return;
  }

  // Sprawdzanie, czy katalog "example" istnieje
  if (SD.exists("/example")) {
    Serial.println("Katalog 'example' istnieje.");
  } else {
    Serial.println("Katalog 'example' nie istnieje.");
  }
}

void loop() {
  // tutaj nic nie umieszczamy
}
```
Wynik:
```
Katalog 'example' istnieje.
```
lub
```
Katalog 'example' nie istnieje.
```

## Głębiej:
Kiedyś zapis nawet najprostszych danych wymagał znajomości struktur systemowych i mnóstwa kodu. Dziś mamy gotowe biblioteki, jak SD.h, które ułatwiają pracę z kartami SD. Alternatywą może być używanie innych układów pamięci lub połączenie z komputerem przez Serial. Pamiętajmy, że sprawdzanie istnienia katalogu to tylko jedna z wielu operacji na systemie plików. Mechanika polega na wysyłaniu komend do kontrolera SD, który sprawdza strukturę plików.

## Zobacz także:
- Dokumentacja biblioteki SD: https://www.arduino.cc/en/Reference/SD
- Tutorial na temat obsługi plików: https://www.arduino.cc/en/Tutorial/LibraryExamples/Files
- Informacje o systemie plików: https://learn.adafruit.com/adafruit-micro-sd-breakout-board-card-tutorial/file-system-info
