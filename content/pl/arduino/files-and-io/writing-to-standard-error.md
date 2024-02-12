---
title:                "Pisanie do standardowego błędu"
aliases:
- /pl/arduino/writing-to-standard-error/
date:                  2024-02-03T19:32:49.675510-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pisanie do standardowego błędu"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie na standardowe wyjście błędów (stderr) w programowaniu Arduino polega na kierowaniu komunikatów o błędach i diagnostyki do osobnego kanału, zapewniając, że nie mieszają się one ze standardowym wyjściem (stdout). Programiści robią to, aby odróżnić normalne wyjścia programu od komunikatów o błędach, co ułatwia debugowanie i analizę logów.

## Jak to zrobić:

Arduino domyślnie nie rozróżnia między standardowym wyjściem a wyjściem błędu, jak to robią konwencjonalne systemy komputerowe. Zarówno metody `Serial.print()` jak i `Serial.println()` zapisują do tego samego wyjścia szeregowego, które zwykle jest wyświetlane w Monitorze Szeregowym Arduino IDE. Jednak możemy emulować zapis na stderr, specjalnie formatując komunikaty o błędach lub kierując je do alternatywnego wyjścia, takiego jak plik na karcie SD lub przez połączenie sieciowe.

Aby emulować stderr, możesz dodać do komunikatów o błędach etykietę typu "ERROR:", aby odróżnić je w Monitorze Szeregowym:

```cpp
void setup() {
  Serial.begin(9600); // Inicjalizacja komunikacji szeregowej z prędkością 9600 bodów
}

void loop() {
  int result = someFunction();
  if (result == -1) {
    // Emulowanie stderr przez dodanie przed komunikatem o błędzie
    Serial.println("ERROR: Funkcja nie została wykonana.");
  } else {
    Serial.println("Funkcja została wykonana pomyślnie.");
  }
  delay(1000); // Oczekuj sekundę przed ponownym rozpoczęciem pętli
}

int someFunction() {
  // Fikcyjna funkcja, która zwraca -1 w przypadku błędu
  return -1;
}
```

Przykładowe wyjście w Monitorze Szeregowym Arduino IDE może wyglądać tak:

```
ERROR: Funkcja nie została wykonana.
```

Dla projektów wymagających bardziej zaawansowanego podejścia, w tym zapisu na różne fizyczne wyjścia, konieczne może być korzystanie z bibliotek stron trzecich lub dodatkowego sprzętu. Na przykład, zapisywanie komunikatów o błędach na karcie SD wymaga biblioteki `SD`:

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin()) {
    Serial.println("ERROR: Inicjalizacja karty SD nie powiodła się!");
    return;
  }
  
  myFile = SD.open("error.log", FILE_WRITE);
  if (myFile) {
    myFile.println("ERROR: Funkcja nie została wykonana.");
    myFile.close(); // Pamiętaj, aby zamknąć plik, aby zapisać zawartość
  } else {
    Serial.println("ERROR: Otwarcie error.log nie powiodło się!");
  }
}

void loop() {
  // Tu znajdowałby się Twój główny kod
}
```

Z takim podejściem fizycznie oddzielasz normalne wyjście programu i komunikaty o błędach, kierując te ostatnie do pliku `error.log` na karcie SD, co umożliwia analizę post mortem bez zaciemniania głównego kanału wyjściowego.
